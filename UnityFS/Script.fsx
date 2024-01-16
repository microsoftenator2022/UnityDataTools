open UnityFS.Reader


#r @"..\UnityFS.Interop\bin\Debug\netstandard2.1\UnityFS.Interop.dll"
#load "Interop.Wrappers.fs"
#load "Interop.UnityArchive.fs"
#load "Interop.SerializedFile.fs"
#load "Interop.UnityFileSystem.fs"
#load "Interop.UnityFile.fs"
#load "TypeTreeNode.fs"
#load "UnityFS.fs"
#load "Reader.fs"
#load "GenericTypeTree.fs"

open UnityFS
open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.TypeTree

IO.init()

let mountPoint = @"archive:\"

let archive = IO.mountArchive @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui" mountPoint

let archiveNodes = IO.getArchiveNodes archive

async {
    for node in archiveNodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let nodePath = $"{mountPoint}{node.Path}"
        printfn "%s" nodePath

        let sf = IO.openSerializedFile nodePath
        let file = IO.openFile nodePath
        let reader = file |> UnityFileReader

        let! objects = IO.getSerializedFileObjectsAsync sf

        // Preload typetrees
        for o in objects do
            let! _ = IO.getTypeTreeAsync sf o.Id
            ()

        let! state = IO.processor.PostAndAsyncReply(fun rc -> IO.GetState rc)

        state.TypeTrees.Count
        |> printfn "%i typetrees"

        let mutable count = 0L

        for o in objects do
            let! tt = IO.getTypeTreeAsync sf o.Id
            let object = TypeTreeObject.get reader [] o.Offset tt
            //printfn "%s : %s" object.Name object.NodeType
            count <- count + 1L
            
        printfn "read %i objects" count
}
|> Async.RunSynchronously

IO.cleanup()
