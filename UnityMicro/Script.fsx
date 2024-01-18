#r @"bin\Debug\net8.0\MicroUtils.dll"
#r @"bin\Debug\net8.0\UnityMicro.dll"

let mountPoint = @"archive:\"

let bundlePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

open MicroUtils

open UnityDataTools
open UnityDataTools.FileSystem

open UnityMicro.TypeTree

type TypeTreeObject = TypeTreeValue<System.Collections.Generic.Dictionary<string, ITypeTreeObject>>

let rec dumpTypeTree (ttn : TypeTreeNode) =
    seq {
        yield sprintf "%s : %s" ttn.Name ttn.Type
        
        for n in ttn.Children do
            yield!
                dumpTypeTree n
                |> Seq.map (sprintf "  %s")
    }
    
UnityFileSystem.Init()

let archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
    let path = $"{mountPoint}{node.Path}"
    printfn "%s" path

    use sf = UnityFileSystem.OpenSerializedFile(path)
    use reader = new UnityFileReader(path, 1024 * 1024 * 1024)
    
    let sw = System.Diagnostics.Stopwatch.StartNew()

    let lines =    
        sf.Objects
        |> Seq.map (fun o -> sf.GetTypeTreeRoot o.Id)
        |> Seq.distinctBy (fun t -> t.Handle)
        |> Seq.collect dumpTypeTree

    System.IO.File.WriteAllLines("dump.txt", lines)

    sw.Stop()

    printfn "Read type trees in %ims" sw.ElapsedMilliseconds

    let mutable i = 0

    for o in sf.Objects do
        let tto = TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id)
        match tto with
        | :? TypeTreeObject as o ->
            if o.Node.Children |> Seq.exists (fun c -> c.Type.StartsWith("PPtr")) then
                let name =
                    match o.Value.TryGetValue("m_Name") with
                    | true, (:? TypeTreeValue<string> as name) -> name.Value
                    | _ -> ""

                if name <> "" then
                    printfn "'%s' (%s : %s)" name o.Node.Name o.Node.Type
                    for n in o.Value.Values |> Seq.where (fun n -> n.Node.Type.StartsWith("PPtr")) do
                        printfn "  %s : %s" n.Node.Name n.Node.Type

        | _ -> ()

        i <- i + 1

    printfn "Read %i objects" i


printfn "Done"

UnityFileSystem.Cleanup()
