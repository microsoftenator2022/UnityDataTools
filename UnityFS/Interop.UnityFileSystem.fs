module [<RequireQualifiedAccess>] UnityFS.Interop.FSharp.UnityFileSystem

open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.Interop.FSharp.Wrappers

let init() =
    (DllWrapper.Init(), ())
    |> handleError
    |> Result.map ignore
    |> function
    | Error ReturnCode.AlreadyInitialized -> Ok ()
    | result -> result
    
let cleanup() =
    (DllWrapper.Cleanup(), ())
    |> handleError
    |> Result.map ignore
    |> function
    | Error ReturnCode.NotInitialized -> Ok ()
    | result -> result
