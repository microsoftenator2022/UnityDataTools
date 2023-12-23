﻿open System.IO

open UnityDataTools.FileSystem
open UnityDataTools.Analyzer
open UnityDataTools.Analyzer.Util

open UnityFS.TypeTree

[<Literal>]
let blockSize = 2 * 1024 * 1024

let dumpObject (node : TypeTreeNode) =
    let rec dumpObject (node : TypeTreeNode) depth = seq {
        for c in node.Children do
            yield sprintf "%s%s" ((Array.create depth ' ') |> System.String) $"{c.Name} : {c.Type}"

            
            
            for g in c.Children do
                yield! dumpObject g (depth + 1)
    }

    dumpObject node 0



[<EntryPoint>]
let main args =

    let path, fileName = if args.Length > 1 then args[0], args[1] else Path.GetDirectoryName(args[0]), Path.GetFileName(args[0])

    UnityFileSystem.Init();

    let mountPoint = $"archive:{Path.DirectorySeparatorChar}"
    let filePath = Path.Join(path, fileName)

    filePath |> printfn "Mounting file %s"

    let archive = UnityFileSystem.MountArchive(filePath, mountPoint);

    archive.Nodes.Count |> printfn "%i nodes"

    let sfProvider = IdProvider<string>()
    let oidProvider = ObjectIdProvider()

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

            let sfId = sfProvider.GetId(fileName.ToLower())

            //use pptrReader = new PPtrAndCrcProcessor(unityFile, reader, path, PPtrAndCrcProcessor.CallbackDelegate(cb))

            //unityFile.ExternalReferences.Count |> printfn "  %i references:"
        
            //for external in unityFile.ExternalReferences do
            //    external.Guid |> printfn "   Guid: %A"
            //    external.Path |> printfn "   Path: %A"
            //    external.Type |> printfn "   Type: %A"

            sf.Objects.Count |> printfn "  %i objects:"

            for o in sf.Objects do
                System.Console.ReadKey() |> ignore

                let root = sf.GetTypeTreeRoot(o.Id)

                getTTObject reader (o.Offset) root
                |> printfn "%A"

                // let index = oidProvider.GetId((sfId, o.Id))
                // printfn $"   object {index}:"

                // let root = sf.GetTypeTreeRoot(o.Id)
                // //let crc = 0
                
                // printfn $"    type: {root.Type} ({o.TypeId})"
                // printfn $"    size: {o.Size}"

                // printfn "    tree:"

                // for line in dumpObject root do
                //     printfn "     %s" line
        
    archive.Dispose()

    UnityFileSystem.Cleanup()
    
    0
