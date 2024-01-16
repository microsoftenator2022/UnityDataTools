module UnityFS.Reader.Reader

open System.IO
open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.Interop.FSharp.Wrappers
open System

type UnityFileStream(file : UnityFile) =
    inherit System.IO.Stream()
    
    let mutable streamPosition = System.Int64.MaxValue

    override _.CanRead = true
    override _.CanSeek = true
    override _.CanWrite = false
    override _.Length = file.Size
    override _.Seek(offset, origin) =
        let origin =
            match origin with
            | System.IO.SeekOrigin.Begin -> SeekOrigin.Begin
            | System.IO.SeekOrigin.Current -> SeekOrigin.Current
            | System.IO.SeekOrigin.End -> SeekOrigin.End
            | x -> x |> int32 |> enum<SeekOrigin>

        let result = UnityFile.seek offset origin file.Handle

        match result with
        | Ok position ->
            streamPosition <- position
            
            position

        | Error err -> System.IO.IOException($"Unexpected return code in {nameof(UnityFile.seek)}: {err}") |> raise

    override _.Read(buffer, offset, count) =
        let resultBuffer = Array.zeroCreate count

        let result = UnityFile.read count resultBuffer file.Handle
        
        match result with
        | Ok (bytes, count) ->
            bytes[0..(count - 1)].CopyTo(buffer, offset)
            count
        | Error err -> System.IO.IOException($"Operation {nameof(UnityFile.read)} unexpected return code: {err}") |> raise
    
    override this.Position
        with get() = streamPosition
        and set value =
            this.Seek(value, System.IO.SeekOrigin.Begin) |> ignore

    override _.Flush() = ()
    override _.SetLength(_) = System.NotSupportedException() |> raise
    override _.Write(_, _, _) = System.NotSupportedException() |> raise

type private ReaderMessage =
| Die
| Read of struct (struct {| Offset : int64; Count : int32 |} * AsyncReplyChannel<Result<byte[], System.Exception>>)

type UnityFileReader(file, ?bufferSize : int32) =
    let stream = new BufferedStream(new UnityFileStream(file), defaultArg bufferSize 4096)

    let getBytes offset (buffer : System.Span<byte>) =
        stream.Seek(offset, System.IO.SeekOrigin.Begin) |> ignore
        stream.Read(buffer)

    let proc = MailboxProcessor.Start(fun inbox ->
        let rec loop() = async {
            match! inbox.Receive() with
            | Die -> return ()
            | Read (args, rc) ->
                try
                    let buffer = Array.zeroCreate args.Count
                    getBytes args.Offset (System.Span buffer) |> ignore

                    rc.Reply (Ok buffer)
                with
                | exn -> rc.Reply (Error exn)
            
            return! loop()
        }

        loop()
    )

    let readBytes count offset =
        proc.PostAndReply (fun rc -> Read ({| Offset = offset; Count = count |}, rc))
        |> function
        | Ok bytes -> bytes
        | Error exn -> raise exn

    member _.ReadArrayAsync offset size destination = async {
        let! result =
            proc.PostAndAsyncReply (fun rc -> Read ({| Offset = offset; Count = size; |}, rc))
        
        return
            result |> Result.map (fun buffer ->
                System.Array.Copy(buffer, destination, size))
    }

    member _.ReadStringAsync offset length = async {
        let! result =
            proc.PostAndAsyncReply (fun rc -> Read ({| Offset = offset; Count = length |}, rc))

        return 
            result |> Result.map (fun buffer ->
                System.Text.Encoding.UTF8.GetString(buffer))
    }

    member _.ReadFloat = readBytes 4 >> BitConverter.ToSingle
    member _.ReadDouble = readBytes 8 >> BitConverter.ToDouble
    member _.ReadInt64 = readBytes 8 >> BitConverter.ToInt64
    member _.ReadUInt64 = readBytes 8 >> BitConverter.ToUInt64
    member _.ReadInt32 = readBytes 4 >> BitConverter.ToInt32
    member _.ReadUInt32 = readBytes 4 >> BitConverter.ToUInt32
    member _.ReadInt16 = readBytes 2 >> BitConverter.ToInt16
    member _.ReadUInt16 = readBytes 2 >> BitConverter.ToUInt16
    member _.ReadInt8 = readBytes 1 >> fun b -> b[0] |> sbyte
    member _.ReadUInt8 = readBytes 1 >> fun b -> b[0]
