#r @"bin\Debug\net8.0\MicroUtils.dll"
#r @"bin\Debug\net8.0\UnityMicro.dll"

open System.IO

open MicroUtils

open UnityDataTools
open UnityDataTools.FileSystem

open UnityMicro.Parsers
open UnityMicro.TypeTree

let mountPoint = @"archive:\"

let bundlePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

type TypeTreeObject = TypeTreeValue<System.Collections.Generic.Dictionary<string, ITypeTreeObject>>

let toValueOption<'a> (microOption : Functional.Option<'a>) : 'a voption =
    if microOption.IsSome then
        ValueSome microOption.Value
    else ValueNone

//let formatAsFileSize (size : int64) : string =
//    if size > 10L * pown 2L 40 then
//        size / (pown 2L 30)
//        |> sprintf "%i TiB"
//    elif size > 10L * pown 2L 30 then
//        size / (pown 2L 30)
//        |> sprintf "%i GiB"
//    elif size > 10L * pown 2L 20 then
//        size / (pown 2L 20)
//        |> sprintf "%i MiB"
//    elif size > 10L * pown 2L 10 then
//        size / (pown 2L 10)
//        |> sprintf "%i KiB"
//    else
//        size |> sprintf "%i B"

//UnityFileSystem.Init()

//let archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

//for n in archive.Nodes do
//    printfn "%s" n.Path
//    n.Size |> formatAsFileSize |> printfn "  Size: %s"
//    printfn "  Flags %A" n.Flags

//    if n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile) then
//        use sf = UnityFileSystem.OpenSerializedFile n.Path
        

//        ()

//UnityFileSystem.Cleanup()

let rec dumpTypeTree (ttn : TypeTreeNode) = seq {
    yield sprintf "%s : %s" ttn.Name ttn.Type
        
    for n in ttn.Children do
        yield!
            dumpTypeTree n
            |> Seq.map (sprintf "  %s")
}

let rec getPPtrs (tto : ITypeTreeObject) = seq {
    match tto with
    | :? TypeTreeValue<PPtr> as v -> yield v.Value
    | :? TypeTreeValue<ITypeTreeObject[]> as arr ->
        yield! arr.Value |> Seq.collect getPPtrs
    | :? TypeTreeObject as o ->
        yield! o.Value.Values |> Seq.collect getPPtrs
    | _ -> ()
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

    printfn "Dumped type trees in %ims" sw.ElapsedMilliseconds

    sw.Restart()

    let mutable i = 0

    for o in sf.Objects do
        let tto = TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id)
        match tto with
        | :? TypeTreeObject as tto ->
            //if tto.Node.Children |> Seq.exists (fun c -> c.Type.StartsWith("PPtr")) then
            let name =
                match tto.Value.TryGetValue("m_Name") with
                | true, (:? TypeTreeValue<string> as name) -> name.Value
                | _ -> ""

            let invalid = Path.GetInvalidFileNameChars()

            let name = 
                name
                |> Seq.map (fun c -> if invalid |> Array.contains c then '_' else c)
                |> Seq.toArray
                |> System.String

            //if name <> "" then
            let filename = Path.Join(node.Path, $"{name}.{o.Id}.{tto.Node.Type}.txt")

            if Directory.Exists(Path.GetDirectoryName(filename)) |> not then
                Directory.CreateDirectory(Path.GetDirectoryName(filename)) |> ignore

            File.WriteAllText(filename, tto.ToString())

        | _ -> ()
        
        i <- i + 1

    sw.Stop()

    printfn "Dumped %i objects in %ims" i sw.ElapsedMilliseconds

    sw.Restart()

    let pptrs =
        sf.Objects
        |> Seq.map (fun o -> TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id))
        |> Seq.collect getPPtrs
        |> Seq.distinct
        |> Seq.cache

    pptrs
    |> Seq.length
    |> printfn "Found %i unique PPtrs"

    let pptrs =
        pptrs
        |> Seq.where (fun pptr -> pptr.FileID = 0 && pptr.PathID <> 0)
        |> Seq.map (fun pptr -> 
            let tto =
                pptr.TryGet(sf, reader)
                |> toValueOption
                |> ValueOption.bind (fun tto -> tto.TryGetObject() |> toValueOption)

            let ttoName =
                tto
                |> ValueOption.bind (fun tto -> tto.TryGetField("m_Name") |> toValueOption)
                |> ValueOption.map(fun f -> f.Invoke().ToString())

            pptr, tto, ttoName)
        |> Seq.cache
    
    seq {

        for (pptr, tto, ttoName) in pptrs do
            match tto with
            | ValueSome tto ->
                let name = match ttoName with ValueSome name -> name | _ -> "<anonymous>"

                yield sprintf "%A -> %s : %s (%s)" pptr name tto.Node.Type (tto.Node.CSharpType.ToString())
            | _ -> ()
    }
    |> fun lines -> File.WriteAllLines("pptrs.txt", lines)

    sw.Stop()

    (pptrs
    |> Seq.length,
    sw.ElapsedMilliseconds)
    ||> printfn "Dumped %i pptrs in %ims" 

printfn "Done"

UnityFileSystem.Cleanup()
