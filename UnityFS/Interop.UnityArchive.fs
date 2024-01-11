module [<RequireQualifiedAccess>] UnityFS.Interop.FSharp.UnityArchive

open UnityFS.Interop
open UnityFS.Interop.FSharp.Wrappers

[<Struct>]
type Node =
  { Size : int64
    Flags : ArchiveNodeFlags 
    Path : string }

let mountArchive mountPoint path =
    DllWrapper.MountArchive(path, mountPoint)
    |> handleError

let getArchiveNodes handle =
    DllWrapper.GetArchiveNodeCount(handle)
    |> handleError
    |> Result.bind (fun count ->
        let pathBuilder = System.Text.StringBuilder(512)

        let nodes =
            seq {
                let mutable error : NativeResult<_> option = None
            
                for i in 0..(count - 1) do
                    if error.IsNone then
                        let returnCode, size, flags =
                            DllWrapper.GetArchiveNode(handle, i, pathBuilder, pathBuilder.Capacity)

                        let result =
                            (returnCode, (size, flags))
                            |> handleError
                            |> function
                            | Error e ->
                                error <- Some (Error e)
                                Error e
                            | Ok (size, flags) ->
                                Ok { Size = size; Flags = flags; Path = pathBuilder.ToString() }
                        yield result

                    pathBuilder.Clear() |> ignore
            } |> Seq.toList

        nodes
        |> List.fold (fun state i ->
            match state, i with
            | Error _, _ -> state
            | _, Error e -> Error e
            | Ok list, Ok i -> Ok (i :: list)
        ) (Ok [])
    )
