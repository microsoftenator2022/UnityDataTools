module [<RequireQualifiedAccess>] UnityFS.Interop.FSharp.UnityFile

open UnityFS.Interop
open UnityFS.Interop.FSharp.Wrappers

let openFile path =
    DllWrapper.OpenFile(path)
    |> handleError

let read (size : int32) buffer handle =
    DllWrapper.ReadFile(handle, size, buffer)
    |> handleError
    |> Result.map (fun s ->
        let s = s |> int32
        struct (buffer, s))

let seek offset origin handle =
    DllWrapper.SeekFile(handle, offset, origin)
    |> handleError

let getSize handle =
    DllWrapper.GetFileSize(handle)
    |> handleError

let readAt offset size buffer handle =
    seek offset SeekOrigin.Begin handle
    |> Result.bind (fun _ -> read size buffer handle)
