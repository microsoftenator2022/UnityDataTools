module UnityFS.TypeTree

open UnityDataTools.FileSystem

[<RequireQualifiedAccess>]
type TypeTreeObject =
| Int32 of int32
| UInt32 of uint32
| Float of single
| Double of double
| SInt16 of int16
| UInt16 of uint16
| SInt64 of int64
| UInt64 of uint64
| SInt8 of int8
| UInt8 of uint8
| Bool of bool
| String of string
| Object of SObject list

and SObject =
  { Name : string
    TypeTree : TypeTreeObject
    EndOffset : int64
    NodeSize : int64 }

type TypeTreeNode
    with
        member this.SizeSafe = if this.Size < 0 then 0 else this.Size

let rec getTTObject (reader : UnityFileReader) (offset : int64) (node : TypeTreeNode) =
    let alignStream (node : TypeTreeNode) (offset : int64) =
        if node.MetaFlags.HasFlag(TypeTreeMetaFlags.AlignBytes)
            || node.MetaFlags.HasFlag(TypeTreeMetaFlags.AnyChildUsesAlignBytes) then
            (offset + 3L) &&& ~~~(3L)
        else
            offset

    let (|BasicType|_|) (node : TypeTreeNode) =
        match node.CSharpType with
        | t when t = typeof<int32> -> reader.ReadInt32(offset) |> TypeTreeObject.Int32 |> Some
        | t when t = typeof<uint32> -> reader.ReadUInt32(offset) |> TypeTreeObject.UInt32 |> Some
        | t when t = typeof<single> -> reader.ReadFloat(offset) |> TypeTreeObject.Float |> Some
        | t when t = typeof<double> -> reader.ReadDouble(offset) |> TypeTreeObject.Double |> Some
        | t when t = typeof<int16> -> reader.ReadInt16(offset) |> TypeTreeObject.SInt16 |> Some
        | t when t = typeof<uint16> -> reader.ReadUInt16(offset) |> TypeTreeObject.UInt16 |> Some
        | t when t = typeof<int64> -> reader.ReadInt64(offset) |> TypeTreeObject.SInt64 |> Some
        | t when t = typeof<uint64> -> reader.ReadUInt64(offset) |> TypeTreeObject.UInt64 |> Some
        | t when t = typeof<int8> -> reader.ReadInt8(offset) |> TypeTreeObject.SInt8 |> Some
        | t when t = typeof<uint8> -> reader.ReadUInt8(offset) |> TypeTreeObject.UInt8 |> Some
        | t when t = typeof<bool> -> reader.ReadUInt8(offset) <> 0uy |> TypeTreeObject.Bool |> Some
        | _ -> None

    let (|String|_|) (node : TypeTreeNode) =
        match node with
        | n when n.CSharpType = typeof<string> || n.Type = "string" ->
            let length = reader.ReadInt32(offset)
            reader.ReadString(offset + 4L, length) |> Some
        | _ -> None

    let (|ManagerReferenceRegistry|_|) (node : TypeTreeNode) =
        if node.IsManagedReferenceRegistry then
            failwith "Not Implemented"
        else None

    match node with
    | BasicType basic ->
        {
            Name = node.Name
            EndOffset = (offset + int64 node.SizeSafe) |> alignStream node
            TypeTree = basic
            NodeSize = node.SizeSafe
        }
    | String s ->
        {
            Name = node.Name
            EndOffset = (offset + 4L + int64 s.Length) |> alignStream node
            TypeTree = s |> TypeTreeObject.String
            NodeSize = node.SizeSafe
        }
    | _ ->
        if node.IsArray then
            let size = node.Children[0] |> getTTObject reader offset
            let offset = size.EndOffset
        
            let size = 
                match size.TypeTree with
                | TypeTreeObject.Int32 size -> size
                | _ -> failwith "Unexpected array size node type"

            let dataNode = node.Children[1]

            let mutable offset = offset
            let elements =
                if size > 0 then
                    seq {
                        for i in 0..(size - 1) do
                            let e = getTTObject reader offset dataNode
                            offset <- e.EndOffset
                            yield e
                    }
                    |> Seq.toList
                else []

            {
                Name = node.Name
                EndOffset = offset |> alignStream node
                TypeTree = TypeTreeObject.Object elements
                NodeSize = node.SizeSafe
            }
        else
            let mutable offset : int64 = offset
            let children =
                seq {
                    for c in node.Children do
                        let cn = getTTObject reader offset c
                        offset <- cn.EndOffset
                        yield cn
                }
                |> Seq.toList
            {
                Name = node.Name
                EndOffset = offset |> alignStream node
                TypeTree = TypeTreeObject.Object children
                NodeSize = node.SizeSafe
            }
