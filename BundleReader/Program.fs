open System.IO

open UnityDataTools.FileSystem

open UnityData.GenericTypeTree

[<Literal>]
let blockSize = 1024 * 1024 * 1024

let printArchiveNodes mountPoint filePath =
    let archive = UnityFileSystem.MountArchive(filePath, mountPoint)

    archive.Nodes.Count |> printfn "%i nodes"

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

            //let mutable objectOffsets : (string * int64 * int64) list = []
            try
                for o in sf.Objects do
                    let root = sf.GetTypeTreeRoot(o.Id)

                    let objectData = TypeTreeObject.get reader [] (o.Offset) root

                    let name =
                        match objectData with
                        | :? TypeTreeValue.Object as o ->
                            o.Value
                            |> Map.tryFind "m_Name"
                            |> Option.bind (function
                            | :? TypeTreeValue<string> as s -> Some s.Value
                            | _ -> None)
                        | _ -> None
                        |> Option.defaultValue ""

                    //objectOffsets <- ($"{name}_{root.Type}", objectData.StartOffset, objectData.EndOffset) :: objectOffsets
                
                    //printfn "   %s \"%s\"" root.Type name
                    ()
                    
            with ex ->
                printfn $"{ex}"

            //if not <| Directory.Exists("offsets") then
            //    Directory.CreateDirectory "offsets" |> ignore

            //let s =
            //    objectOffsets
            //    |> Seq.rev
            //    |> Seq.map (fun (n, x, y) -> $"{n}:{x},{y}")
            //    |> String.concat "\n"

            //File.WriteAllText($"offsets\\{node.Path}.offsets.txt", s)

    archive.Dispose()

[<EntryPoint>]
let main args =

    let path, fileName = if args.Length > 1 then args[0], args[1] else Path.GetDirectoryName(args[0]), Path.GetFileName(args[0])

    UnityFileSystem.Init()

    let mountPoint = $"archive:{Path.DirectorySeparatorChar}"
    let filePath = Path.Join(path, fileName)

    printArchiveNodes mountPoint filePath

    // let files = Directory.GetFiles args[0] |> Seq.where (fun s -> s.EndsWith ".unit" |> not) |> Seq.toArray

    // let mutable i = 0;
    // let mutable failed : string list = []
    // for f in files do
    //     // f |> printfn "Mounting file %s"
    //     try
    //         printArchiveNodes mountPoint f
    //     with _ ->
    //         eprintfn $"Exception occured in {f}"
            
    //         failed <- f :: failed

    //     i <- i + 1
    //     eprintfn $"{i}/{files.Length}"

    // printfn "Failed to load bundles:"
    
    // failed
    // |> Seq.iter (printfn "%s")

    UnityFileSystem.Cleanup()

    0
