#r @"bin\Debug\net8.0\MicroUtils.dll"
#r @"bin\Debug\net8.0\UnityMicro.dll"

open System.IO

open MicroUtils

open UnityDataTools
open UnityDataTools.FileSystem

open UnityMicro.Parsers
open UnityMicro.TypeTree

let mountPoint = @"archive:/"

let bundlePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

let bufferSize = 4 * 1024
let maxBufferSize = 32 * 1024 * 1024

type TypeTreeObject = TypeTreeValue<System.Collections.Generic.Dictionary<string, ITypeTreeObject>>

let toValueOption<'a> (microOption : Functional.Option<'a>) : 'a voption =
    if microOption.IsSome then
        ValueSome microOption.Value
    else ValueNone

let toMicroOption<'a> (valueOption : ValueOption<'a>) : Functional.Option<'a> =
    match valueOption with
    | ValueSome some -> Functional.Option.Some(some)
    | ValueNone -> Functional.Option<'a>.None

let tryGetObject (tto : ITypeTreeObject) : TypeTreeObject voption =
    tto.TryGetObject()
    |> toValueOption

let tryGetField<'a> fieldName (tto : TypeTreeObject) : 'a voption =
    tto.TryGetField<'a>(fieldName)
    |> toValueOption
    |> ValueOption.map (fun f -> f.Invoke())

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

let getStreamingInfos (tto: ITypeTreeObject) =
    tto.Find(fun c -> c.Node.Type = "StreamingInfo")
    |> Seq.choose (function :? TypeTreeObject as o -> Some o | _ -> None)

let invalidFileChars = Path.GetInvalidFileNameChars()

let formatAsFileSize (size : int64) : string =
    if size > 10L * pown 2L 40 then
        size / (pown 2L 30)
        |> sprintf "%i TiB"
    elif size > 10L * pown 2L 30 then
        size / (pown 2L 30)
        |> sprintf "%i GiB"
    elif size > 10L * pown 2L 20 then
        size / (pown 2L 20)
        |> sprintf "%i MiB"
    elif size > 10L * pown 2L 10 then
        size / (pown 2L 10)
        |> sprintf "%i KiB"
    else
        size |> sprintf "%i B"

//UnityFileSystem.Init()

//let archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

//for n in archive.Nodes do
    //printfn "%s" n.Path
    //n.Size |> formatAsFileSize |> printfn "  Size: %s"
    //printfn "  Flags %A" n.Flags

//UnityFileSystem.Cleanup()

let testGetStream() =

    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let path = $"{mountPoint}{node.Path}"
        printfn "%s" path

        use sf = UnityFileSystem.OpenSerializedFile(path)
        use reader = new UnityFileReader(path, bufferSize, maxBufferSize)

        let sis =
            sf.Objects
            |> Seq.map (fun o -> TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id, sf))
            |> Seq.map (fun tto -> tto, getStreamingInfos tto)
            |> Seq.where (fun (_, sis) -> sis |> Seq.isEmpty |> not)
            |> Seq.cache

        //sis
        //|> Seq.iter (fun (tto, sis) ->
        //    let name = 
        //        tto.TryGetObject()
        //        |> toValueOption
        //        |> ValueOption.bind (fun tto -> tto.TryGetField<string>("m_Name") |> toValueOption)
        //        |> ValueOption.map (fun f -> f.Invoke())
        //        |> ValueOption.defaultValue tto.Node.Name

        //    (name, sis) |> printfn "%A")

        let (tto, streamInfos) =
            sis |> Seq.head

        let name = 
            tto
            |> tryGetObject
            |> ValueOption.bind (tryGetField "m_Name")
            |> ValueOption.defaultValue tto.Node.Name

        printfn "%s : %s (%s) StreamingInfo:" name tto.Node.Type (tto.Node.CSharpType.ToString())

        let si =
            streamInfos
            |> Seq.head

        let (resPath, offset, length) =
            (si |> tryGetField<string> "path",
            si |> tryGetField<uint64> "offset",
            si |> tryGetField<uint32> "size")
            |||> ValueOption.map3 (fun x y z -> x, y, z)
            |> ValueOption.get

        let resPath = resPath.Replace($"{path}/", "")
        let resPath = $"{mountPoint}{resPath}"

        printfn "Resource path: %s" resPath

        use resReader = new UnityFileReader(resPath, length |> int32)

        let arr = Array.zeroCreate<byte> (length |> int32)

        resReader.ReadArray(offset |> int64, length |> int32, arr)

        arr |> printfn "Bytes: %A"

    UnityFileSystem.Cleanup()

let dump outputDir =
    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let path = $"{mountPoint}{node.Path}"
        printfn "%s" node.Path
        
        node.Size |> formatAsFileSize |> printfn "  Size: %s"
        printfn "  Flags %A" node.Flags

        use sf = UnityFileSystem.OpenSerializedFile(path)
        use reader = new UnityFileReader(path, bufferSize, maxBufferSize)
        
        let sw = System.Diagnostics.Stopwatch.StartNew()

        //let trees =
        //    sf.Objects
        //    |> Seq.map (fun o -> sf.GetTypeTreeRoot o.Id)
        //    |> Seq.distinctBy (fun t -> t.Handle)
        //    |> Seq.groupBy (fun t -> t.Type)
        //    |> Seq.collect (fun (t, tts) ->
        //        if tts |> Seq.length = 1 then
        //            [t, tts |> Seq.head] |> Seq.ofList
        //        else
        //            tts
        //            |> Seq.mapi (fun i tt -> $"{t}{i}", tt))

        //let typeTreesDumpPath = Path.Join(outputDir, "TypeTrees")

        //if Directory.Exists typeTreesDumpPath |> not then
        //    Directory.CreateDirectory typeTreesDumpPath |> ignore

        //for (n, tree) in trees do
        //    File.WriteAllLines(Path.Join(typeTreesDumpPath, $"{n}.txt"), dumpTypeTree tree)

        //sw.Stop()

        //printfn "Dumped type trees in %ims" sw.ElapsedMilliseconds

        //sw.Restart()

        //let mutable i = 0

        //for o in sf.Objects do
        //    let tto = TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id)
        //    match tto with
        //    | :? TypeTreeObject as tto ->
        //        let name =
        //            match tto.Value.TryGetValue("m_Name") with
        //            | true, (:? TypeTreeValue<string> as name) -> name.Value
        //            | _ -> ""

        //        let name =
        //            name
        //            |> Seq.map (fun c -> if invalidFileChars |> Array.contains c then '_' else c)
        //            |> Seq.toArray
        //            |> System.String

        //        let filename = Path.Join(outputDir, node.Path, $"{name}.{o.Id}.{tto.Node.Type}.txt")

        //        if Directory.Exists(Path.GetDirectoryName(filename)) |> not then
        //            Directory.CreateDirectory(Path.GetDirectoryName(filename)) |> ignore

        //        File.WriteAllText(filename, tto.ToString())

        //    | _ -> ()

        
        //    i <- i + 1

        //sw.Stop()

        //printfn "Dumped %i objects in %ims" i sw.ElapsedMilliseconds

        //sw.Restart()

        let pptrs =
            sf.Objects
            |> Seq.map (fun o -> TypeTreeObject.Get(reader, MicroStack.Empty, o.Offset, sf.GetTypeTreeRoot o.Id, sf))
            |> Seq.collect getPPtrs
            |> Seq.distinct
            |> Seq.cache

        pptrs
        |> Seq.length
        |> printfn "Found %i unique PPtrs"

        let pptrs =
            pptrs
            //|> Seq.where (fun pptr -> pptr.PathID <> 0)
            |> Seq.map (fun pptr -> 
                let tto =
                    pptr.TryDereference(
                        (fun sfp -> (if sfp = path then ValueSome sf else ValueNone) |> toMicroOption),
                        (fun sf -> (if sf.Path = path then reader else null)))
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
        |> fun lines -> File.WriteAllLines(Path.Join(outputDir, "pptrs.txt"), lines)

        sw.Stop()

        (pptrs
        |> Seq.length,
        sw.ElapsedMilliseconds)
        ||> printfn "Dumped %i PPtrs in %ims" 

    printfn "Done"

    UnityFileSystem.Cleanup()

let extRefs() =
    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let nodePath = $"{mountPoint}{node.Path}"
        node.Path |> printfn "%s"
        printfn "External references:"

        use sf = UnityFileSystem.OpenSerializedFile(nodePath)

        for er in sf.ExternalReferences |> Seq.toArray do
            let pathAsBytes = er.Path.ToCharArray() |> Array.map(fun c -> c |> byte)
            printfn $"  Path: {er.Path}"
            printfn $"    Guid: {er.Guid}"
            printfn $"    Type: {er.Type}"

    UnityFileSystem.Cleanup()

if fsi.CommandLineArgs.Length > 1 && fsi.CommandLineArgs[1] <> "" then
    dump fsi.CommandLineArgs[1]