module UnityFS.IO

open UnityFS.Interop
open UnityFS.Interop.FSharp
open UnityFS.Interop.FSharp.Wrappers
open UnityFS.Interop.FSharp.TypeTree

[<Struct>]
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

[<Struct>]
type Message =
| Init
| Cleanup
| GetState of getStateArgs : AsyncReplyChannel<ProcessorState>
| MountArchive of mountArchiveArgs : struct {| MountPoint : string; Path : string; ReplyWith: AsyncReplyChannel<UnityArchiveHandle> |}
| GetNodes of getNodesArgs : struct (UnityArchiveHandle * AsyncReplyChannel<System.Collections.Generic.IReadOnlyList<UnityArchive.Node>>)
| OpenSerializedFile of openSerializedFileArgs : struct (string * AsyncReplyChannel<SerializedFileHandle>)
| GetObjectsInfo of getObjectsInfoArgs : struct (SerializedFileHandle * AsyncReplyChannel<ObjectInfo[]>)
| GetTypeTreeHandle of getTypeTreeHandleArgs : struct (SerializedFileHandle * int64 * AsyncReplyChannel<TypeTreeHandle>)
| GetTypeTree of getTypeTreeArgs : struct (TypeTreeHandle * AsyncReplyChannel<TypeTree.TypeTreeNode>)

let processor = MailboxProcessor.Start (fun inbox ->
    let inline getResultValue (result : NativeResult<_>) =
        match result with
        | Ok ok -> ok
        | Error rc ->
            failwith $"Operation failed in IO process with return code: {rc}"

    let rec loop (state : ProcessorState) = async {
        let! msg = inbox.Receive()
        
        let state=
            match msg with
            | GetState rc ->
                rc.Reply state

                state

            | Init ->
                UnityFileSystem.init()
                |> function
                | Ok ok -> ok
                | Error ReturnCode.AlreadyInitialized -> ()
                | Error returnCode -> failwith $"Operation failed in IO process with return code: {returnCode}"

                state

            | Cleanup ->
                state.Dispose()

                UnityFileSystem.cleanup()
                |> function
                | Ok ok -> ok
                | Error ReturnCode.NotInitialized -> ()
                | Error returnCode -> failwith $"Operation failed in IO process with return code: {returnCode}"

                state

            | MountArchive args ->
                let handle, state =
                    state.Archives
                    |> Map.tryFind args.MountPoint
                    |> Option.map (fun handle -> handle, state)
                    |> Option.defaultWith (fun () ->
                        let handle =
                            UnityArchive.mountArchive args.MountPoint args.Path
                            |> getResultValue
                        handle, { state with Archives = state.Archives |> Map.add args.MountPoint handle }
                    )

                args.ReplyWith.Reply handle

                state

            | GetNodes (handle, rc) ->
                let nodes =
                    UnityArchive.getArchiveNodes handle
                    |> getResultValue

                rc.Reply nodes

                state

            | OpenSerializedFile (path, rc) ->
                let handle, state =
                    state.SerializedFiles
                    |> Map.tryFind path
                    |> Option.map (fun sf -> sf, state)
                    |> Option.defaultWith (fun () ->
                        let handle =
                            SerializedFile.openFile path
                            |> getResultValue

                        handle, { state with SerializedFiles = state.SerializedFiles |> Map.add path handle }
                    )

                rc.Reply handle

                state

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
                let tt, state =
                    state.TypeTrees
                    |> Map.tryFind handle
                    |> Option.map (fun handle -> handle, state)
                    |> Option.defaultWith (fun () ->
                        let tt = (TypeTreeNode.getTypeTree handle |> getResultValue)

                        tt, { state with TypeTrees = state.TypeTrees |> Map.add handle tt }
                    )

                rc.Reply tt

                state

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
