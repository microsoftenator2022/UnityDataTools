namespace rec UnityData.TypeTree

open Newtonsoft.Json
open UnityDataTools.FileSystem

type SerializedObject = { Properties : Map<string, TypeTreeObject> }

[<Struct>]
type TypeTreeObject =
  { [<JsonIgnore>] Node : TypeTreeNode
    [<JsonIgnore>] Ancestors : TypeTreeNode list
    Value : TypeTreeValue
    StartOffset : int64
    EndOffset : int64 }
with
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

[<RequireQualifiedAccess>]
module TypeTreeValue =
    [<Struct>]
    type TypeTreeValue =
    | Skipped
    | SInt32 of i32 : int32
    | UInt32 of u32 :uint32
    | Float of f32 : single
    | Double of f64 : double
    | SInt16 of i16 : int16
    | UInt16 of u16 : uint16
    | SInt64 of i64 : int64
    | UInt64 of u64 : uint64
    | SInt8 of i8 : int8
    | UInt8 of u8 : uint8
    | Bool of b : bool
    | String of s : string
    | IntegralArray of ia : IntegralArray
    | Array of a : TypeTreeObject[]
    | Object of so : SerializedObject
    // with
    //     static member inline Of x = SInt32 x
    //     static member inline Of x = UInt32 x
    //     static member inline Of x = Float x
    //     static member inline Of x = Double x
    //     static member inline Of x = SInt16 x
    //     static member inline Of x = UInt16 x
    //     static member inline Of x = SInt64 x
    //     static member inline Of x = UInt64 x
    //     static member inline Of x = SInt8 x
    //     static member inline Of x = UInt8 x
    //     static member inline Of x = Bool x
    //     static member inline Of x = String x
    //     static member inline Of x = IntegralArray x
    //     static member inline Of x = Array x
    //     static member inline Of x = Object x

    //     static member inline ($) (t : TypeTreeValue, _ : int32) = match t with TypeTreeValue.SInt32 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : uint32) = match t with TypeTreeValue.UInt32 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : single) = match t with TypeTreeValue.Float v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : double) = match t with TypeTreeValue.Double v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : int16) = match t with TypeTreeValue.SInt16 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : uint16) = match t with TypeTreeValue.UInt32 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : int64) = match t with TypeTreeValue.SInt64 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : uint64) = match t with TypeTreeValue.UInt64 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : int8) = match t with TypeTreeValue.SInt8 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : uint8) = match t with TypeTreeValue.UInt8 v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : bool) = match t with TypeTreeValue.Bool v -> Some v | _ -> None
    //     static member inline ($) (t : TypeTreeValue, _ : string) = match t with TypeTreeValue.String v -> Some v | _ -> None

    // let inline tryGetValue (t : TypeTreeValue) : 'a option = t $ Unchecked.defaultof<'a>

    let tryIntegralType (reader : UnityFileReader) (offset : int64) (node : TypeTreeNode) =
        match node.CSharpType with
        | t when t = typeof<int32> -> reader.ReadInt32(offset) |> SInt32 |> ValueSome
        | t when t = typeof<uint32> -> reader.ReadUInt32(offset) |> UInt32 |> ValueSome
        | t when t = typeof<single> -> reader.ReadFloat(offset) |> Float |> ValueSome
        | t when t = typeof<double> -> reader.ReadDouble(offset) |> Double |> ValueSome
        | t when t = typeof<int16> -> reader.ReadInt16(offset) |> SInt16 |> ValueSome
        | t when t = typeof<uint16> -> reader.ReadUInt16(offset) |> UInt16 |> ValueSome
        | t when t = typeof<int64> -> reader.ReadInt64(offset) |> SInt64 |> ValueSome
        | t when t = typeof<uint64> -> reader.ReadUInt64(offset) |> UInt64 |> ValueSome
        | t when t = typeof<int8> -> reader.ReadInt8(offset) |> SInt8 |> ValueSome
        | t when t = typeof<uint8> -> reader.ReadUInt8(offset) |> UInt8 |> ValueSome
        | t when t = typeof<bool> -> reader.ReadUInt8(offset) <> 0uy |> Bool |> ValueSome
        | _ -> ValueNone

    let tryString (reader : UnityFileReader) (offset : int64) (node : TypeTreeNode) =
        if node.Type = "string" then
            let length = reader.ReadInt32(offset)

            TypeTreeValue.String (
                if length > 0 then
                    reader.ReadString(offset + 4L, length)
                else ""
            )
            |> ValueSome
        else ValueNone

    let f x = TypeTreeValue.UInt32 x

[<RequireQualifiedAccess>]
type TypeTreeValue = TypeTreeValue.TypeTreeValue

[<RequireQualifiedAccess>]
module IntegralArray =
    [<Struct>]
    type IntegralArray =
    | SInt32 of i32 : int32[]
    | UInt32 of u32 : uint32[]
    | Float of f32 : single[]
    | Double of f64 : double[]
    | SInt16 of i16 : int16[]
    | UInt16 of u16 : uint16[]
    | SInt64 of i64 : int64[]
    | UInt64 of u64 : uint64[]
    | SInt8 of i8 : int8[]
    | UInt8 of u8 : uint8[]
    | Bool of b : bool[]

[<RequireQualifiedAccess>]
type IntegralArray = IntegralArray.IntegralArray

[<RequireQualifiedAccess>]
module TypeTreeObject =   

    type TypeTreeNode with
        member node.SizeSafe =
            match node with
            | _ when node.Size > 0 -> int64 node.Size
            | _ -> 0L

    let toIntegralArray (dataTypeNode : TypeTreeNode) (elements : TypeTreeObject seq) =
        if not dataTypeNode.IsBasicType then ValueNone
        elif elements |> Seq.exists (fun e -> e.NodeType <> dataTypeNode.Type) then ValueNone
        else
            let elements = elements |> Seq.cache

            let array =
                elements
                |> Seq.length
                |>
                match dataTypeNode.CSharpType with
                | t when t = typeof<int32> -> Array.zeroCreate<int32> >> IntegralArray.SInt32
                | t when t = typeof<uint32> -> Array.zeroCreate<uint32> >> IntegralArray.UInt32
                | t when t = typeof<single> -> Array.zeroCreate<single> >> IntegralArray.Float
                | t when t = typeof<double> -> Array.zeroCreate<double> >> IntegralArray.Double
                | t when t = typeof<int16> -> Array.zeroCreate<int16> >> IntegralArray.SInt16
                | t when t = typeof<uint16> -> Array.zeroCreate<uint16> >> IntegralArray.UInt16
                | t when t = typeof<int64> -> Array.zeroCreate<int64> >> IntegralArray.SInt64
                | t when t = typeof<uint64> -> Array.zeroCreate<uint64> >> IntegralArray.UInt64
                | t when t = typeof<int8> -> Array.zeroCreate<int8> >> IntegralArray.SInt8
                | t when t = typeof<uint8> -> Array.zeroCreate<uint8> >> IntegralArray.UInt8
                | t when t = typeof<bool> -> Array.zeroCreate<bool> >> IntegralArray.Bool
                | _ -> failwith "Invalid"
                |> ignore

            ValueNone

    [<return: Struct>]
    let (|String|_|) (reader, _, offset, node) = TypeTreeValue.tryString reader offset node

    [<return: Struct>]
    let (|IntegralType|_|) (reader, _, offset, node) = TypeTreeValue.tryIntegralType reader offset node

    [<return: Struct>]
    let (|IntegralArray|_|) (reader : UnityFileReader, _, offset : int64, node : TypeTreeNode) =
        match node with
        | _ when node.IsArray && node.IsBasicType ->
            
            let sizeNode = node.Children[0]

            if (not sizeNode.IsLeaf) && sizeNode.Size <> 4 then
                failwith "Unexpected array size"

            let size = reader.ReadInt32(offset)
            let size = if size < 0 then 0 else size
            let offset = offset + 4L
            let dataNode = node.Children[1]

            let elementSize = dataNode.SizeSafe

            // let array = Array.zeroCreate<

            for i in 0..(size - 1) do
                TypeTreeValue.tryIntegralType reader offset dataNode
                |> ignore

            ValueNone
        | _ -> ValueNone


    [<return: Struct>]
    let (|Array|_|) (reader : UnityFileReader, ancestors, offset : int64, node : TypeTreeNode) =
        match node with
        | _ when node.IsArray ->
            let sizeNode = node.Children[0]

            if (not sizeNode.IsLeaf) || sizeNode.Size <> 4 then
                failwith "Unexpected array size"

            let size = reader.ReadInt32(offset)
            let size = if size < 0 then 0 else size
            let offset = offset + 4L
            let dataNode = node.Children[1]

            let elements =
                if size > 0 then
                    (0, offset)
                    |> Seq.unfold (fun (i, offset) ->
                        if i < size then
                            let e = get reader (node :: ancestors) offset dataNode
                            Some (e, (i + 1, e.NextNodeOffset))
                        else None)
                    |> Seq.toArray
                else
                    Array.empty
            
            let value =
                match dataNode, elements with
                // | IntegralArray ia ->
                //     TypeTreeValue.IntegralArray ia
                | _ -> TypeTreeValue.Array elements

            {
                Value = value
                Node = node
                Ancestors = ancestors
                StartOffset = offset
                EndOffset = if size = 0 then offset else (elements |> Array.last).NextNodeOffset
            }
            |> ValueSome

        | _ -> ValueNone

    let rec get reader (ancestors : TypeTreeNode list) offset node : TypeTreeObject =
        let get = get reader
        try
            match (reader, ancestors, offset, node) with
            | IntegralType v ->
              { Value = v
                Node = node
                Ancestors = ancestors
                StartOffset = offset
                EndOffset = offset + node.SizeSafe }

            | String (TypeTreeValue.String s) ->
              { Value = TypeTreeValue.String s
                Node = node
                Ancestors = ancestors
                StartOffset = offset
                EndOffset = offset + 4L + int64 s.Length }
            | Array arr -> arr
            // | _ when node.IsArray ->
            //     let sizeNode = node.Children[0]
                
            //     if (not sizeNode.IsLeaf) && sizeNode.Size <> 4 then
            //         failwith "Unexpected array size"
            //     else
            //         let size = reader.ReadInt32(offset)
            //         let size = if size < 0 then 0 else size
            //         let offset = offset + 4L
            //         let dataNode = node.Children[1]

            //         let elements =
            //             if size > 0 then
            //                 (0, offset)
            //                 |> Seq.unfold (fun (i, offset) ->
            //                     if i < size then
            //                         let e = get (node :: ancestors) offset dataNode
            //                         Some (e, (i + 1, e.NextNodeOffset))
            //                     else None)
            //                 |> Seq.toArray
            //             else
            //                 Array.empty
                    
            //         let value =
            //             if dataNode.IsBasicType then
            //                 elements
            //                 |> Seq.map (fun e -> e.Value)
            //                 |> Seq.toArray
            //                 |> TypeTreeValue.IntegralArray
            //             else
            //                 TypeTreeValue.Array elements
                            
            //         {
            //             Value = value
            //             Node = node
            //             Ancestors = ancestors
            //             StartOffset = offset
            //             EndOffset = if size = 0 then offset else (elements |> Array.last).NextNodeOffset
            //         }

            | _ when node.IsManagedReferenceRegistry ->
                if ancestors.Length > 0 then
                    {
                        Value = TypeTreeValue.Skipped
                        Node = node
                        Ancestors = ancestors
                        StartOffset = offset
                        EndOffset = offset + node.SizeSafe
                    }
                else
                    failwith $"Unhandled node type {node.Type}"

            | _ ->
                let props =
                    (offset, node.Children |> Seq.toList)
                    |> Seq.unfold (fun (offset, children) ->
                        match children with
                        | [] -> None
                        | head :: tail ->
                            let child = get (node :: ancestors) offset head
                            Some (child, (child.NextNodeOffset, tail)))
                    |> Seq.cache

                {
                    Value = TypeTreeValue.Object
                      { Properties =
                        props
                        |> Seq.map (fun child -> child.Name, child)
                        |> Map.ofSeq }
                    Node = node
                    Ancestors = ancestors
                    StartOffset = offset
                    EndOffset = if node.Children.Count = 0 then offset else (props |> Seq.last).NextNodeOffset
                }
                
        with ex ->
            let dumpNode (node : TypeTreeNode) =
                let rec dumpNode (node : TypeTreeNode) =
                    seq {
                        yield sprintf $"  {node.Name} : {node.Type} ({node.CSharpType})"

                        for c in node.Children do
                            for line in dumpNode c do
                                yield sprintf $"  {line}"
                    }

                dumpNode node
                |> String.concat "\n  "

            failwith
              ( $"{ex}\n"
                + "Exception in node:\n"
                + $"  Offset: {offset}\n"
                + dumpNode node )
