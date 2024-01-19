open System.IO


#r @"bin\Debug\net8.0\MicroUtils.dll"
#r @"bin\Debug\net8.0\UnityMicro.dll"

let mountPoint = @"archive:\"

let bundlePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

open MicroUtils

open UnityDataTools
open UnityDataTools.FileSystem

open UnityMicro.TypeTree

type TypeTreeObject = TypeTreeValue<System.Collections.Generic.Dictionary<string, ITypeTreeObject>>

let rec dumpTypeTree (ttn : TypeTreeNode) = seq {
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

    let trees =
        sf.Objects
        |> Seq.map (fun o -> sf.GetTypeTreeRoot o.Id)
        |> Seq.distinctBy (fun t -> t.Handle)
        |> Seq.groupBy (fun t -> t.Type)
        |> Seq.collect (fun (t, tts) ->
            if tts |> Seq.length = 1 then
                [t, tts |> Seq.head] |> Seq.ofList
            else
                tts
                |> Seq.mapi (fun i tt -> $"{t}{i}", tt))

    if Directory.Exists "trees" |> not then
        Directory.CreateDirectory "trees" |> ignore

    for (n, tree) in trees do
        File.WriteAllLines(Path.Join("trees", $"{n}.txt"), dumpTypeTree tree)

    sw.Stop()

    printfn "Read type trees in %ims" sw.ElapsedMilliseconds

    let mutable i = 0

    for o in sf.Objects do
        let tto = TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id)
        match tto with
        | :? TypeTreeObject as tto ->
            if tto.Node.Children |> Seq.exists (fun c -> c.Type.StartsWith("PPtr")) then
                let name =
                    match tto.Value.TryGetValue("m_Name") with
                    | true, (:? TypeTreeValue<string> as name) -> name.Value
                    | _ -> ""

                if name <> "" then
                    let filename = Path.Join(node.Path, $"{name}.dump.txt")

                    if Directory.Exists(Path.GetDirectoryName(filename)) |> not then
                        Directory.CreateDirectory(Path.GetDirectoryName(filename)) |> ignore

                    File.WriteAllText(filename, tto.ToString())

        | _ -> ()
        
        i <- i + 1

    printfn "Read %i objects" i


printfn "Done"

UnityFileSystem.Cleanup()