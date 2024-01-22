open System.Diagnostics


#r @"bin\Debug\net8.0\MicroUtils.dll"
#r @"bin\Debug\net8.0\UnityMicro.dll"

open System.IO

type MicroOption<'a> = MicroUtils.Functional.Option<'a>

open UnityDataTools
open UnityDataTools.FileSystem

open UnityMicro.Parsers
open UnityMicro.TypeTree

let mountPoint = @"archive:/"

let bundlePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

let bufferSize = 4 * 1024
let maxBufferSize = 32 * 1024 * 1024

type TypeTreeObject = TypeTreeValue<System.Collections.Generic.Dictionary<string, ITypeTreeObject>>

let toValueOption<'a> (microOption : MicroOption<'a>) : 'a voption =
    if microOption.IsSome then
        ValueSome microOption.Value
    else ValueNone

let toMicroOption<'a> (valueOption : ValueOption<'a>) : MicroOption<'a> =
    match valueOption with
    | ValueSome some -> MicroOption.Some(some)
    | ValueNone -> MicroOption<'a>.None

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

let invalidFileChars = Path.GetInvalidFileNameChars()


let getName (tto : ITypeTreeObject) =
    match tto with
    | :? TypeTreeObject as tto ->
        let name =
            match tto.Value.TryGetValue("m_Name") with
            | true, (:? TypeTreeValue<string> as name) -> name.Value
            | _ -> ""

        name
        |> Seq.map (fun c -> if invalidFileChars |> Array.contains c then '_' else c)
        |> Seq.toArray
        |> System.String
    | _ -> tto.Node.Name
    
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

let printArchiveFiles() =

    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for n in archive.Nodes do
        printfn "%s" n.Path
        n.Size |> formatAsFileSize |> printfn "  Size: %s"
        printfn "  Flags %A" n.Flags

    UnityFileSystem.Cleanup()

printArchiveFiles()

let dumpStreamData dumpPath =

    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let path = $"{mountPoint}{node.Path}"
        printfn "%s" path

        use sf = UnityFileSystem.OpenSerializedFile(path)
        use sfReader = new UnityFileReader(path, bufferSize, maxBufferSize)

        let sw = Stopwatch.StartNew()
        let mutable i = 0

        let mutable fileReader : (string * UnityFileReader) voption = ValueNone

        let getReader rPath =
            match fileReader with
            | ValueSome (readerPath, fileReader) when readerPath = rPath ->
                ValueSome fileReader
            | maybeReader ->
                match maybeReader with
                | ValueSome (_, reader) ->
                    reader.Dispose()
                | _ -> ()

                fileReader <- ValueSome (rPath, new UnityFileReader(rPath, bufferSize))
                sfReader |> ValueSome
        try
            for objectInfo in sf.Objects do
                let tto = TypeTreeObject.Get(sf, sfReader, objectInfo)

                let sis =
                    tto.Find(fun tto -> tto :? TypeTreeValue<StreamingInfo>)
                    |> Seq.map (fun si -> (si :?> TypeTreeValue<StreamingInfo>).Value)
                    |> Seq.cache
                    |> Seq.mapi (fun i si ->
                        let name = tto |> getName
                        let filePath = Path.Join($"{name}.{objectInfo.Id}", $"{i}.{tto.Node.Type}")

                        filePath, si)

                for (filePath, si) in sis do
                    match si.TryGetData(getReader >> toMicroOption) |> toValueOption with
                    | ValueSome arr ->
                        let filePath = Path.Join(dumpPath, "StreamData", filePath)
                        let dirPath = Path.GetDirectoryName(filePath)

                        if Directory.Exists(dirPath) |> not then
                            Directory.CreateDirectory(dirPath) |> ignore

                        printfn $"Dumping {si.Size} bytes to {filePath}"

                        File.WriteAllBytes(filePath, arr)
                        i <- i + 1
                    | _ -> ()
        finally
            match fileReader with
            | ValueSome (path, reader) ->
                reader.Dispose()
                fileReader <- ValueNone
            | _ -> ()

            sw.Stop()

        printfn "Dumped stream data x %i in %ims" i sw.ElapsedMilliseconds

        //try
        //    for si in
        //        sf.Objects
        //        |> Seq.map (fun o -> TypeTreeObject.Get(sf, sfReader, o))
        //        |> Seq.cache
        //        |> Seq.collect (fun tto ->
        //            tto.Find(fun tto -> tto :? TypeTreeValue<StreamingInfo>))
        //        |> Seq.map (fun si -> (si :?> TypeTreeValue<StreamingInfo>).Value)
        //        |> Seq.cache do
            
        //        si.TryGetData(fun requestedPath ->
        //            match fileReader with
        //            | ValueSome (readerPath, fileReader) when readerPath = requestedPath ->
        //                ValueSome fileReader
        //            | maybeReader ->
        //                match maybeReader with
        //                | ValueSome (_, reader) ->
        //                    reader.Dispose()
        //                | _ -> ()

        //                fileReader <- ValueSome (requestedPath, new UnityFileReader(requestedPath, bufferSize, debug = true))
        //                sfReader |> ValueSome
        //            |> toMicroOption)
        //        |> toValueOption
        //        |> function
        //        | ValueSome arr ->
        //            ()//let outDir = Path.Join(dumpPath, si.)
        //        | ValueNone -> ()
        //finally
        //    match fileReader with
        //    | ValueSome (path, reader) ->
        //        reader.Dispose()
        //        fileReader <- ValueNone
        //    | _ -> ()

    UnityFileSystem.Cleanup()

let dump outputDir =
    UnityFileSystem.Init()

    use archive = UnityFileSystem.MountArchive(bundlePath, mountPoint)

    for node in archive.Nodes |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile)) do
        let path = $"{mountPoint}{node.Path}"
        printfn "open %s" path

        use sf = UnityFileSystem.OpenSerializedFile(path)
        use reader = new UnityFileReader(path, bufferSize, maxBufferSize)
        
        let sw = Stopwatch.StartNew()

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

        let typeTreesDumpPath = Path.Join(outputDir, "TypeTrees")

        if Directory.Exists typeTreesDumpPath |> not then
            Directory.CreateDirectory typeTreesDumpPath |> ignore

        for (n, tree) in trees do
            File.WriteAllLines(Path.Join(typeTreesDumpPath, $"{n}.txt"), dumpTypeTree tree)

        sw.Stop()

        printfn "Dumped type trees in %ims" sw.ElapsedMilliseconds

        sw.Restart()

        let mutable i = 0

        for o in sf.Objects do
            let tto = TypeTreeObject.Get(sf, reader, o)
            match tto with
            | :? TypeTreeObject as tto ->
                let name =
                    match tto.Value.TryGetValue("m_Name") with
                    | true, (:? TypeTreeValue<string> as name) -> name.Value
                    | _ -> ""

                let name =
                    name
                    |> Seq.map (fun c -> if invalidFileChars |> Array.contains c then '_' else c)
                    |> Seq.toArray
                    |> System.String

                let filename = Path.Join(outputDir, node.Path, $"{name}.{o.Id}.{tto.Node.Type}.txt")

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
            |> Seq.map (fun o -> TypeTreeObject.Get(sf, reader, o))
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
                        (fun readerPath -> (if readerPath = path then MicroOption.Some(reader) else MicroOption<UnityFileReader>.None)))
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
    //dump fsi.CommandLineArgs[1]
    dumpStreamData fsi.CommandLineArgs[1]