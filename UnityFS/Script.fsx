#r @"bin\Debug\netstandard2.1\UnityFS.Interop.dll"
#r @"bin\Debug\netstandard2.1\UnityFS.dll"

open UnityFS.Interop
open UnityFS.IO

let mountPoint = @"archive:\"

init()

let archive = mountArchive @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui" mountPoint
let nodes = getArchiveNodes archive

for node in nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
    printfn "Node: %s%s" mountPoint node.Path

    let sf = openSerializedFile $"{mountPoint}{node.Path}"
    printfn "Opened serialized file"
    
    let task =
        getTypeTreesAsync sf
        |> Async.StartImmediateAsTask

    task.Result.Length
    |> printfn "%i trees"

cleanup()

