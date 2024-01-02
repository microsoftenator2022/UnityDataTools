open System.IO

open UnityDataTools.FileSystem

open UnityData.GenericTypeTree
// open UnityData.TypeTreeDump

[<Literal>]
let blockSize = 1024 * 1024 * 1024

// let dumpObject (node : TypeTreeNode) =
//    let rec dumpObject (node : TypeTreeNode) depth = seq {
//        for c in node.Children do
//            yield sprintf "%s%s" ((Array.create depth ' ') |> System.String) $"{c.Name} : {c.Type}"

            
            
//            for g in c.Children do
//                yield! dumpObject g (depth + 1)
//    }

//    dumpObject node 0

[<EntryPoint>]
let main args =

    let path, fileName = if args.Length > 1 then args[0], args[1] else Path.GetDirectoryName(args[0]), Path.GetFileName(args[0])

    UnityFileSystem.Init()

    let mountPoint = $"archive:{Path.DirectorySeparatorChar}"
    let filePath = Path.Join(path, fileName)

    filePath |> printfn "Mounting file %s"

    let archive = UnityFileSystem.MountArchive(filePath, mountPoint)

    archive.Nodes.Count |> printfn "%i nodes"

    let cb (objectId : int64) (fileId : int32) (pathId : int64) (propertyPath : string) (propertyType : string) =
        printfn $"oid: {objectId}, fid: {fileId}, pid: {pathId}, pp: {propertyPath}, pt: {propertyType}"
        0 // crc?

    for node in archive.Nodes do
        let nodePath = $"{mountPoint}{node.Path}"

        nodePath |> printfn " %s"

        node.Size
        |> printfn "  Size: %i"

        node.Flags
        |> printfn "  Flags: %A"

        if node.Flags.HasFlag(ArchiveNodeFlags.SerializedFile) then
            use sf = UnityFileSystem.OpenSerializedFile(nodePath)
            use reader = new UnityFileReader(nodePath, blockSize)

            sf.Objects.Count |> printfn "  %i objects:"

            for o in sf.Objects do
                let root = sf.GetTypeTreeRoot(o.Id)

                let tt = TypeTreeObject.get reader [] (o.Offset) root

                tt |> printfn "%A"

                // dumpObject tt
                // |> ignore
                // |> printfn "%s"
        
    archive.Dispose()

    UnityFileSystem.Cleanup()

    0
