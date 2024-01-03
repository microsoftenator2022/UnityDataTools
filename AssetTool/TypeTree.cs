using System.Collections;

using UnityDataTools.FileSystem;

namespace AssetTool
{
    using System.Diagnostics;

    using Util;

    public interface ITypeTreeObject
    {
        TypeTreeNode Node { get; }
        MicroStack<TypeTreeNode> Ancestors { get; }
        long StartOffset { get; }
        long EndOffset { get; }
    }

    public readonly record struct TypeTreeValue<T>(
        TypeTreeNode Node,
        MicroStack<TypeTreeNode> Ancestors,
        long StartOffset,
        long EndOffset,
        T Value) : ITypeTreeObject { }

    public readonly record struct TypeTreeIgnored(
        TypeTreeNode Node,
        MicroStack<TypeTreeNode> Ancestors,
        long StartOffset,
        long EndOffset) : ITypeTreeObject { }

    public static class TypeTreeExtensions
    {
        public static long SizeSafe(this TypeTreeNode node) => node.Size < 0 ? 0 : node.Size;

        public static string Name(this ITypeTreeObject obj) => obj.Node.Name;
        public static string NodeType(this ITypeTreeObject obj) => obj.Node.Type;
        public static long NodeSize(this ITypeTreeObject obj) => obj.Node.SizeSafe();
        public static long NextNodeOffset(this ITypeTreeObject obj) => TypeTreeUtil.AlignOffset(obj.EndOffset, obj.Node);
    }

    namespace Util
    {
        public static class TypeTreeUtil
        {
            public static long AlignOffset(long offset, TypeTreeNode node)
            {
                if (node.MetaFlags.HasFlag(TypeTreeMetaFlags.AlignBytes)
                    ||
                    node.MetaFlags.HasFlag(TypeTreeMetaFlags.AnyChildUsesAlignBytes)
                    )
                {
                    return (offset + 3L) & (~(3L));
                }

                return offset;
            }

            public static Option<ITypeTreeObject> TryIntegralValue(
                UnityFileReader reader,
                MicroStack<TypeTreeNode> ancestors,
                long offset,
                TypeTreeNode node)
            {
                ITypeTreeObject value<T>(T value) =>
                    new TypeTreeValue<T>(node, ancestors, offset, offset + node.SizeSafe(), value);

                if (!node.IsBasicType)
                    return Option<ITypeTreeObject>.None;

                return node.CSharpType switch
                {
                    var t when t == typeof(int) => Option.Some(value(reader.ReadInt32(offset))),
                    var t when t == typeof(uint) => Option.Some(value(reader.ReadUInt32(offset))),
                    var t when t == typeof(float) => Option.Some(value(reader.ReadFloat(offset))),
                    var t when t == typeof(double) => Option.Some(value(reader.ReadDouble(offset))),
                    var t when t == typeof(short) => Option.Some(value(reader.ReadInt16(offset))),
                    var t when t == typeof(ushort) => Option.Some(value(reader.ReadUInt16(offset))),
                    var t when t == typeof(long) => Option.Some(value(reader.ReadInt64(offset))),
                    var t when t == typeof(ulong) => Option.Some(value(reader.ReadUInt64(offset))),
                    var t when t == typeof(sbyte) => Option.Some(value(reader.ReadInt8(offset))),
                    var t when t == typeof(byte) => Option.Some(value(reader.ReadUInt8(offset))),
                    var t when t == typeof(bool) => Option.Some(value(reader.ReadUInt8(offset) != 0)),
                    _ => Option<ITypeTreeObject>.None
                };
            }

            public static Option<ITypeTreeObject> TryString(
                UnityFileReader reader,
                MicroStack<TypeTreeNode> ancestors,
                long startOffset,
                TypeTreeNode node)
            {
                ITypeTreeObject value(long endOffset, string value) =>
                    new TypeTreeValue<string>(node, ancestors, startOffset, endOffset, value);

                if (node.Type != "string")
                    return Option<ITypeTreeObject>.None;

                var length = reader.ReadInt32(startOffset);
                var offset = startOffset + 4L;

                (length, var s) =
                    length > 0 ?
                    (length, reader.ReadString(offset, length)) :
                    (0, "");

                return Option.Some(value(offset + length, s));
            }

            public static Option<ITypeTreeObject> TryArray(
                UnityFileReader reader,
                MicroStack<TypeTreeNode> ancestors,
                long startOffset,
                TypeTreeNode node)
            {
                ITypeTreeObject value(long endOffset, System.Array value) =>
                    new TypeTreeValue<System.Array>(node, ancestors, startOffset, endOffset, value);

                if (!node.IsArray)
                    return Option<ITypeTreeObject>.None;

                var sizeNode = node.Children[0];
                if (!sizeNode.IsLeaf || sizeNode.Size != 4)
                    throw new Exception("Unexpected array size node");

                var length = reader.ReadInt32(startOffset);
                var offset = startOffset + 4L;

                length = length > 0 ? length : 0;

                var dataNode = node.Children[1];

                if (dataNode.IsBasicType)
                {
                    var array = System.Array.CreateInstance(dataNode.CSharpType, length);

                    if (length > 0)
                        reader.ReadArray(offset, length, array);
                    
                    offset += (dataNode.SizeSafe() * (long)length);

                    return Option.Some(value(offset, array));
                }

                var elements = new ITypeTreeObject[length];

                if (length > 0)
                {
                    for (var i = 0; i < length; i++)
                    {
                        elements[i] = TypeTreeObject.Get(reader, (node, ancestors), offset, dataNode);

                        offset = elements[i].NextNodeOffset();
                    }
                }

                return Option.Some(value(offset, elements));
            }

            public static TypeTreeValue<Dictionary<string, ITypeTreeObject>> GetObject(
                UnityFileReader reader,
                MicroStack<TypeTreeNode> ancestors,
                long startOffset,
                TypeTreeNode node)
            {
                TypeTreeValue<Dictionary<string, ITypeTreeObject>> value(
                    long endOffset,
                    Dictionary<string, ITypeTreeObject> value) =>
                    new(node, ancestors, startOffset, endOffset, value);

                var offset = startOffset;

                var children = new ITypeTreeObject[node.Children.Count];

                for (var i = 0; i < children.Length; i++)
                {
                    var childNode = node.Children[i];
                    var childOffset = offset;

                    var child = TypeTreeObject.Get(reader, (node, ancestors), childOffset, childNode);
                    children[i] = child;
                    offset = child.NextNodeOffset();
                }

                var properties = new Dictionary<string, ITypeTreeObject>();

                foreach (var c in children)
                {
                    properties.Add(c.Name(), c);
                }

                return value(offset, properties);
            }
        }
    }

    static class TypeTreeObject
    {
        public static ITypeTreeObject Get(
            UnityFileReader reader,
            MicroStack<TypeTreeNode> ancestors,
            long offset,
            TypeTreeNode node)
        {
            try
            {
                var result = TypeTreeUtil.TryIntegralValue(reader, ancestors, offset, node)
                    .OrElseWith(() => TypeTreeUtil.TryString(reader, ancestors, offset, node))
                    .OrElseWith(() => TypeTreeUtil.TryArray(reader, ancestors, offset, node))
                    .OrElseWith(() =>
                    {
                        if (node.IsManagedReferenceRegistry)
                        {
                            if (!node.IsLeaf && ancestors.Count() > 0)
                            {
                                return Option.Some(new TypeTreeIgnored(node, ancestors, offset, node.SizeSafe()) as ITypeTreeObject);
                            }

                            throw new Exception($"{node.Type} not implemented");
                        }

                        return Option<ITypeTreeObject>.None;
                    })
                    .DefaultWith(() => TypeTreeUtil.GetObject(reader, ancestors, offset, node));

                return result;
            }
            catch (Exception ex)
            {
                Debugger.Break();

                throw new Exception(
                    $"Exception in node {node.Type} \"{node.Name}\" at offset {offset}:\n  {ex.Message}", ex);
            }
        }
    }
}
