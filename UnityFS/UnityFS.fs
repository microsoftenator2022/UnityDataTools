module UnityFS.IO

open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.Interop.FSharp.Wrappers
open UnityFS.Interop.FSharp.TypeTree

type ProcessorState =
  { Archives : Map<string, UnityArchiveHandle>
    SerializedFiles : Map<string, SerializedFileHandle>
    Files : Map<string, UnityFileHandle>
    TypeTrees : Map<TypeTreeHandle, TypeTree.TypeTreeNode> }
with
    static member Empty = { Archives = Map.empty; Files = Map.empty; SerializedFiles = Map.empty; TypeTrees = Map.empty }
    
    member this.Dispose() =
        for f in this.Files |> Map.values do
            f.Dispose()

        for sf in this.SerializedFiles |> Map.values do
            sf.Dispose()

        for a in this.Archives |> Map.values do
            a.Dispose()

type Message =
| Init
| Cleanup
| MountArchive of {| MountPoint : string; Path : string; ReplyWith: AsyncReplyChannel<UnityArchiveHandle> |}
| GetNodes of (UnityArchiveHandle * AsyncReplyChannel<System.Collections.Generic.IReadOnlyList<UnityArchive.Node>>)
| OpenSerializedFile of (string * AsyncReplyChannel<SerializedFileHandle>)
| GetObjectsInfo of (SerializedFileHandle * AsyncReplyChannel<ObjectInfo[]>)
| GetTypeTreeHandle of (SerializedFileHandle * int64 * AsyncReplyChannel<TypeTreeHandle>)
| GetTypeTree of (TypeTreeHandle * AsyncReplyChannel<TypeTree.TypeTreeNode>)

let processor = MailboxProcessor.Start (fun inbox ->
    let throw rc =
        match rc with
        | ReturnCode.Success -> ()
        | _ ->
            failwith $"Operation failed in IO process with return code: {rc}"
    
    let inline getResultValue (result : NativeResult<_>) =
        match result with
        | Ok ok -> ok
        | Error rc ->
            throw rc
            Unchecked.defaultof<'a>

    let rec loop (state : ProcessorState) = async {
        let! msg = inbox.Receive()
        
        let state=
            match msg with
            | Init ->
                UnityFileSystem.init()
                |> function
                | Ok ok -> ok
                | Error ReturnCode.AlreadyInitialized -> ()
                | Error returnCode -> throw returnCode

                state

            | Cleanup ->
                state.Dispose()

                UnityFileSystem.cleanup()
                |> function
                | Ok ok -> ok
                | Error ReturnCode.NotInitialized -> ()
                | Error returnCode -> throw returnCode

                state

            | MountArchive args ->
                let handle =
                    UnityArchive.mountArchive args.MountPoint args.Path
                    |> getResultValue

                args.ReplyWith.Reply handle

                { state with Archives = state.Archives |> Map.add args.MountPoint handle }

            | GetNodes (handle, rc) ->
                let nodes =
                    UnityArchive.getArchiveNodes handle
                    |> getResultValue

                rc.Reply nodes

                state

            | OpenSerializedFile (path, rc) ->
                let handle =
                    SerializedFile.openFile path
                    |> getResultValue

                rc.Reply handle

                { state with SerializedFiles = state.SerializedFiles |> Map.add path handle }

            | GetObjectsInfo (handle, rc) ->
                let ois =
                    SerializedFile.getObjects handle
                    |> getResultValue

                rc.Reply ois

                state

            | GetTypeTreeHandle (sf, oid, rc) ->
                let handle =
                    SerializedFile.getTypeTreeHandle oid sf
                    |> getResultValue

                rc.Reply handle

                state

            | GetTypeTree (handle, rc) ->
                let tt =
                    TypeTreeNode.getTypeTree handle
                    |> getResultValue

                rc.Reply tt

                { state with TypeTrees = state.TypeTrees |> Map.add handle tt }

        return! loop state
    }
    loop ProcessorState.Empty
)

let init() = processor.Post Init

let cleanup() = processor.Post Cleanup

let mountArchive path mountPoint = processor.PostAndReply (fun rc ->
    MountArchive {| MountPoint = mountPoint; Path = path; ReplyWith = rc |})

let getArchiveNodes archive = processor.PostAndReply (fun rc -> GetNodes (archive, rc))

let openSerializedFile path = processor.PostAndReply (fun rc -> OpenSerializedFile (path, rc))

let getSerializedFileObjectsAsync serializedFile = processor.PostAndAsyncReply (fun rc -> GetObjectsInfo (serializedFile, rc))

let getSerializedFileObjects serializedFile = processor.PostAndReply (fun rc -> GetObjectsInfo (serializedFile, rc))

let getTypeTreeAsync serializedFile objectId = async {
    let! handle = processor.PostAndAsyncReply (fun rc -> GetTypeTreeHandle (serializedFile, objectId, rc))
    return! processor.PostAndAsyncReply (fun rc -> GetTypeTree (handle, rc))
}

let getTypeTree serializedFile objectId =
    let handle = processor.PostAndReply (fun rc -> GetTypeTreeHandle (serializedFile, objectId, rc))
    processor.PostAndReply (fun rc -> GetTypeTree (handle, rc))

let getTypeTreesAsync serializedFile = async {
    let! objects = getSerializedFileObjectsAsync serializedFile

    let trees =
        objects
        |> Seq.map (fun o -> getTypeTreeAsync serializedFile o.Id)
        |> Async.Parallel

    return! trees
}