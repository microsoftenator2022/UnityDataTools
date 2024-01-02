#r "bin/Debug/net8.0/BundleReader.dll"
#r "bin/Debug/net8.0/UnityFileSystem.dll"
#r "nuget: Newtonsoft.Json"

open System.IO
open UnityDataTools.FileSystem
open Newtonsoft.Json
open UnityData.GenericTypeTree
// open UnityData.TypeTreeDump

[<Literal>]
let blockSize = 1024 * 1024 * 1024

let args = fsi.CommandLineArgs |> Array.skip 1

UnityFileSystem.Init()

let mountPoint = $"archive:{Path.DirectorySeparatorChar}"
let filePath = args[0]

let archive = UnityFileSystem.MountArchive(filePath, mountPoint)

let outDir = "output"

if not <| Directory.Exists outDir then
    Directory.CreateDirectory outDir |> ignore

for node in archive.Nodes do
    if node.Flags.HasFlag(ArchiveNodeFlags.SerializedFile) then
        let nodePath = $"{mountPoint}{node.Path}"

        use sf = UnityFileSystem.OpenSerializedFile(nodePath)
        use reader = new UnityFileReader(nodePath, blockSize)

        printfn "Serializing %s" nodePath

        let objects = 
            seq {
                for o in sf.Objects do
                    let root = sf.GetTypeTreeRoot(o.Id)

                    let objectData = TypeTreeObject.get reader [] (o.Offset) root

                    // objectData.DumpString()
                    // |> printfn "%s"

                    // System.Console.ReadKey() |> ignore

                    yield objectData
            }
            |> Seq.toArray 
        
        objects
        |> Seq.iteri (fun i o -> File.WriteAllText(Path.Join(outDir, $"{node.Path}_{i}.json"), JsonConvert.SerializeObject(o, Formatting.Indented)))

archive.Dispose()
UnityFileSystem.Cleanup()
