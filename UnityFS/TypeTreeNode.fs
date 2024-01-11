module UnityFS.Interop.FSharp.TypeTree

open System
open System.Text

open UnityFS.Interop
open UnityFS.Interop.FSharp.Wrappers

let (|CSharpType|_|) = function
| "int" | "SInt32" | "TypePtr" -> Some typeof<int32>
| "unsigned int" | "UInt32" -> Some typeof<uint32>
| "float" -> Some typeof<float32>
| "double" -> Some typeof<double>
| "SInt16" -> Some typeof<int16>
| "UInt16" -> Some typeof<uint16>
| "SInt64" -> Some typeof<int64>
| "UInt64" | "FileSize" -> Some typeof<uint64>
| "SInt8" -> Some typeof<int8>
| "UInt8" | "char" -> Some typeof<uint8>
| "bool" -> Some typeof<bool>
| "string" -> Some typeof<string>
| _ -> None

type private PerThread =
    [<ThreadStatic; DefaultValue>]
    static val mutable private NodeTypeBuilderInstance : StringBuilder

    static member NodeTypeBuilder =
        if PerThread.NodeTypeBuilderInstance = null then
            PerThread.NodeTypeBuilderInstance <- StringBuilder(512)

        PerThread.NodeTypeBuilderInstance

    [<ThreadStatic; DefaultValue>]
    static val mutable private NodeNameBuilderInstance : StringBuilder

    static member NodeNameBuilder =
        if PerThread.NodeNameBuilderInstance = null then
            PerThread.NodeNameBuilderInstance <- StringBuilder(512)

        PerThread.NodeNameBuilderInstance

type TypeTreeNode =
  { Handle : TypeTreeHandle
    FirstChildIndex : int
    NextNodeIndex : int
    Children : TypeTreeNode[]
    Type : string
    Name : string
    Size : int
    Offset : int
    Flags : TypeTreeFlags
    MetaFlags : TypeTreeMetaFlags }
with
    member this.CSharpType =
        match this.Type, this.Size with
        | CSharpType t, _ -> t
        | _, 8 -> typeof<int64>
        | _, 4 -> typeof<int32>
        | _, 2 -> typeof<int16>
        | _, 1 -> typeof<sbyte>
        | _ -> typeof<obj>

    member this.IsLeaf = this.Children.Length = 0
    member this.IsBasicType = this.IsLeaf && this.Size > 0
    member this.IsArray = this.Flags.HasFlag(TypeTreeFlags.IsArray)
    member this.IsManagedReferenceRegistry = this.Flags.HasFlag(TypeTreeFlags.IsManagedReferenceRegistry)
    member this.ConstantSize =
        not (this.IsArray || this.CSharpType = typeof<string>)
        && this.Children |> Seq.forall (fun c -> c.ConstantSize)

[<RequireQualifiedAccess>]
module TypeTreeNode =
    let rec getNodeInfo (handle : TypeTreeHandle) index =
        let typeBuilder = PerThread.NodeTypeBuilder
        let nameBuilder = PerThread.NodeNameBuilder

        let returnCode, offset, size, flags, metaFlags, firstChildIndex, nextIndex =
            DllWrapper.GetTypeTreeNodeInfo(handle.Handle, index, typeBuilder, typeBuilder.Capacity, nameBuilder, nameBuilder.Capacity)

        let rec getChildren currentIndex (children) =
            match currentIndex with
            | 0 -> Ok children
            | _ ->
                getNodeInfo handle currentIndex
                |> Result.bind (fun currentNode -> (currentNode :: children) |> getChildren currentNode.NextNodeIndex)

        (returnCode, ())
        |> handleError
        |> Result.bind (fun _ -> getChildren firstChildIndex [])
        |> Result.map (fun children ->
            let children = children |> Seq.rev |> Seq.toArray

            {
                Handle = handle
                FirstChildIndex = firstChildIndex
                NextNodeIndex = nextIndex
                Children = children
                Type = typeBuilder.ToString()
                Name = nameBuilder.ToString()
                Size = size
                Offset = offset
                Flags = flags
                MetaFlags = metaFlags
            }
        )

    let getTypeTree handle = getNodeInfo handle 0
