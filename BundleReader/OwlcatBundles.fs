module OwlcatBundles

open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions

let getDependencies path =
    let dir =
        if File.Exists path then
            Path.GetDirectoryName path
        elif Directory.Exists path then
            path
        else
            failwith "Directory does not exist"

    let json =
        let p = Path.Join(dir, "dependencylist.json")
        if File.Exists p then
            File.ReadAllText p
            |> JsonValue.Parse
        else
            failwith $"File {p} does not exist"

    json?BundleToDependencies.Properties
    |> Seq.map (fun (key, value) ->
        key,
        value.AsArray()
        |> Array.map (fun v -> v.ToString().Replace("\"", "") |> System.String))
    |> Map.ofSeq

let getBundleDependencies path =
    (getDependencies path)[Path.GetFileName path]

let getBundleDependenciesRecursive path =
    let lookup = getDependencies path
    
    let rec getDeps fs =
        let deps =
            fs
            |> Seq.collect (fun f-> lookup[f])
            |> Seq.append fs
            |> Seq.distinct

        if (deps |> Seq.length) > (fs |> Seq.length) then
            getDeps deps
        else deps

    getDeps [Path.GetFileName path]
