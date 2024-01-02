module UnityData.GenericTypeTree

open UnityDataTools.FileSystem
open Newtonsoft.Json

type ITypeTreeObject =
    abstract member Node : TypeTreeNode
    [<JsonIgnore>] abstract member Ancestors : TypeTreeNode list
    abstract member StartOffset : int64
    abstract member EndOffset : int64

[<AutoOpen>]
module ITypeTreeObject =
    type ITypeTreeObject with
        member this.NodeSize = this.Node.Size
        member this.NodeType = this.Node.Type
        member this.Name = this.Node.Name
        member this.NextNodeOffset : int64 =
            let align (node : TypeTreeNode) (offset : int64) =
                if node.MetaFlags.HasFlag(TypeTreeMetaFlags.AlignBytes)
                    || node.MetaFlags.HasFlag(TypeTreeMetaFlags.AnyChildUsesAlignBytes) then
                    (offset + 3L) &&& ~~~(3L)
                else
                    offset

            this.EndOffset |> align this.Node

[<Struct>]
type TypeTreeValue<'a> =
  { Value : 'a
    Node : TypeTreeNode
    [<JsonIgnore>] Ancestors : TypeTreeNode list
    Offset : int64
    EndOffset : int64 }
    
    interface ITypeTreeObject with
        override this.Node = this.Node
        override this.Ancestors = this.Ancestors
        override this.StartOffset = this.Offset
        override this.EndOffset = this.EndOffset

[<RequireQualifiedAccess>]
module TypeTreeObject =
    type TypeTreeNode with
        member node.SizeSafe =
            match node with
            | _ when node.Size > 0 -> int64 node.Size
            | _ -> 0L

    [<return: Struct>]
    let (|Integral|_|) (reader : UnityFileReader, ancestors : TypeTreeNode list, offset : int64, node : TypeTreeNode) =
        let value v =
          { Value = v
            Node = node
            Offset = offset
            EndOffset = offset + node.SizeSafe
            Ancestors = ancestors } :> ITypeTreeObject
        
        match node with
        | _ when node.CSharpType = typeof<int32> -> reader.ReadInt32(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<uint32> -> reader.ReadUInt32(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<single> -> reader.ReadFloat(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<double> -> reader.ReadDouble(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<int16> -> reader.ReadInt16(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<uint16> -> reader.ReadUInt16(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<int64> -> reader.ReadInt64(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<uint64> -> reader.ReadUInt64(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<int8> -> reader.ReadInt8(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<uint8> -> reader.ReadUInt8(offset) |> value |> ValueSome
        | _ when node.CSharpType = typeof<bool> -> reader.ReadUInt8(offset) <> 0uy |> value |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let (|String|_|) (reader : UnityFileReader, ancestors, offset, node) =
        let value endOffset v =
          { Value = v
            Node = node
            Offset = offset
            EndOffset = endOffset
            Ancestors = ancestors }

        if node.Type = "string" then
            let length = reader.ReadInt32(offset)
            let offset = offset + 4L

            let length, s =
                if length > 0 then
                    length, reader.ReadString(offset, length)
                else 0, ""

            value (offset + int64 length) s |> ValueSome
        else ValueNone

    [<return: Struct>]
    let rec (|Array|_|) (reader : UnityFileReader, ancestors : TypeTreeNode list, offset : int64, node : TypeTreeNode) =
        let value endOffset v =
          { Value = v
            Node = node
            Offset = offset
            EndOffset = endOffset
            Ancestors = ancestors }

        if node.IsArray then
            let sizeNode = node.Children[0]

            if (not sizeNode.IsLeaf) || sizeNode.Size <> 4 then
                failwith "Unexpected array size node"

            let size = reader.ReadInt32(offset)
            let offset = offset + 4L
            let size = if size < 0 then 0 else size

            let dataNode = node.Children[1]

            if dataNode.IsBasicType then
                let array = System.Array.CreateInstance(dataNode.CSharpType, size)
                
                if size > 0 then
                    reader.ReadArray(offset, size, array)

                value (offset + (dataNode.SizeSafe * int64 size)) array
            else
                let elements =
                    if size > 0 then
                        (0, offset)
                        |> Seq.unfold (fun (i, offset) ->
                            if i < size then
                                let e : ITypeTreeObject = get reader (node :: ancestors) offset dataNode
                                Some (e, (i + 1, e.NextNodeOffset))
                            else None)
                        |> Seq.toArray
                    else Array.empty
                
                value (if size = 0 then offset else (elements |> Array.last).NextNodeOffset) elements
            |> ValueSome

        else ValueNone

    and getObject (reader : UnityFileReader) (ancestors : TypeTreeNode list) (offset : int64) (node : TypeTreeNode) =
        let value endOffset v =
          { Value = v
            Node = node
            Offset = offset
            EndOffset = endOffset
            Ancestors = ancestors }
        
        let properties =
            (offset, node.Children |> Seq.toList)
            |> Seq.unfold (fun (offset, children) ->
                match children with
                | [] -> None
                | head :: tail ->
                    let child = get reader (node :: ancestors) offset head
                    Some (child, (child.NextNodeOffset, tail)))
            |> Seq.cache

        properties
        |> Seq.map (fun child -> child.Name, child)
        |> Map.ofSeq
        |> value (if node.Children.Count = 0 then offset else (properties |> Seq.last).NextNodeOffset)

    and get (reader : UnityFileReader) (ancestors : TypeTreeNode list) (offset : int64) (node : TypeTreeNode) : ITypeTreeObject =
        try
            match reader, ancestors, offset, node with
            | Integral i -> i
            | String s -> s
            | Array arr -> arr
            | _ when node.IsManagedReferenceRegistry ->
                if ancestors.Length > 0 then
                    { new ITypeTreeObject with
                        member _.Node = node
                        member _.StartOffset = offset
                        member _.EndOffset = offset + node.SizeSafe
                        member _.Ancestors = ancestors }
                else
                    failwith "Managed reference registry parsing not implemented"
            | _ -> getObject reader ancestors offset node
        with ex ->
            failwith $"{ex}"
