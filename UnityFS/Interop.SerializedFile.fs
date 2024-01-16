module [<RequireQualifiedAccess>] UnityFS.Interop.FSharp.SerializedFile

open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.Interop.FSharp.Wrappers

let openFile path =
    DllWrapper.OpenSerializedFile(path)
    |> handleError

let getObjects (handle : SerializedFileHandle) =
    DllWrapper.GetObjectCount(handle)
    |> handleError
    |> Result.bind (fun count ->
        let ois = Array.zeroCreate<ObjectInfo> count
        
        if count = 0 then
            Ok ois
        else
            let rc = DllWrapper.GetObjectInfo(handle, ois, count)
            
            handleError (rc, ())
            |> Result.map (fun _ -> ois))

let getTypeTreeHandle objectId serializedFile =
    DllWrapper.GetTypeTree(serializedFile, objectId)
    |> handleError
    |> Result.map (fun h -> { Handle = h; SerializedFile = serializedFile })
