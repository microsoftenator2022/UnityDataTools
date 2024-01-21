namespace UnityMicro.TypeTree;

using System.Diagnostics;
using System.Text;

using MicroUtils;
using MicroUtils.Functional;

using UnityDataTools.FileSystem;

using UnityMicro.Parsers;

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
    T Value) : ITypeTreeObject
{
    static IEnumerable<(int, string)> GetToStringLines(ITypeTreeObject obj, int indentLevel = 0)
    {
        yield return (indentLevel, $"{obj.Node.Name} : {obj.Node.Type} ({obj.Node.CSharpType})");
        if (obj is TypeTreeValue<Dictionary<string, ITypeTreeObject>> tto)
        {
            yield return (indentLevel, "{");
            foreach (var n in tto.Value.Values)
                foreach (var (il, line) in GetToStringLines(n, indentLevel + 1))
                    yield return (il, $"{line}");
            yield return (indentLevel, "}");
            yield break;
        }

        var type = obj.GetType();

        if (type.IsConstructedGenericType && type.GenericTypeArguments.Any(at => typeof(System.Array).IsAssignableFrom(at)))
        {
            var arr = type.GetProperty(nameof(Value))?.GetValue(obj) as System.Array;
            //yield return $"{arr?.Length ?? 0} items";

            if (arr?.Length is 0)
            {
                yield return (indentLevel, "[]");
                yield break;
            }

            yield return (indentLevel, "[");
         
            if (obj is TypeTreeValue<ITypeTreeObject[]> objArr)
            {
                foreach (var n in objArr.Value)
                    foreach (var (il, line) in GetToStringLines(n, indentLevel + 1))
                        yield return (il, $"{line},");
            }
            else if (arr is not null)
            {
                yield return (indentLevel + 1, $"{string.Join(", ", arr.Cast<object>().Select(o => o.ToString()))}");
            }

            yield return (indentLevel, "]");
            yield break;
        }

        if (type.IsConstructedGenericType)
        {
            var v = type.GetProperty(nameof(Value))?.GetValue(obj);

            if (v is ITypeTreeObject o)
            {
                foreach (var line in GetToStringLines(o, indentLevel + 1))
                    yield return (indentLevel, $"{line}");
            }
            else
                yield return (indentLevel + 1, $"= {v?.ToString() ?? "NULL"}");
        }
    }

    static string GetIndent(int level) => String.Concat(Enumerable.Repeat("  ", level));

    public override string ToString()
    {
        var sb = new StringBuilder();
        foreach (var (indentLevel, s) in GetToStringLines(this))
        {
            var indent = GetIndent(indentLevel);

            var lines = s.ReplaceLineEndings("\n").Split("\n");

            foreach (var line in lines)
            {
                sb.AppendLine($"{indent}{line}");
            }
        }

        return sb.ToString();
    }
}

public readonly record struct TypeTreeIgnored(
    TypeTreeNode Node,
    MicroStack<TypeTreeNode> Ancestors,
    long StartOffset,
    long EndOffset) : ITypeTreeObject
{ }

public static class TypeTreeExtensions
{
    public static long SizeSafe(this TypeTreeNode node) => node.Size < 0 ? 0 : node.Size;

    public static string Name(this ITypeTreeObject obj) => obj.Node.Name;
    public static string NodeType(this ITypeTreeObject obj) => obj.Node.Type;
    public static long NodeSize(this ITypeTreeObject obj) => obj.Node.SizeSafe();
    public static long NextNodeOffset(this ITypeTreeObject obj) => TypeTreeUtil.AlignOffset(obj.EndOffset, obj.Node);
}

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
        TypeTreeNode node,
        SerializedFile sf)
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
                elements[i] = TypeTreeObject.Get(reader, (node, ancestors), offset, dataNode, sf);

                offset = elements[i].NextNodeOffset();
            }
        }

        return Option.Some(value(offset, elements));
    }

    public static TypeTreeValue<Dictionary<string, ITypeTreeObject>> GetObject(
        UnityFileReader reader,
        MicroStack<TypeTreeNode> ancestors,
        long startOffset,
        TypeTreeNode node,
        SerializedFile sf)
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

            var child = TypeTreeObject.Get(reader, (node, ancestors), childOffset, childNode, sf);
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

public static class TypeTreeObject
{
    internal static ITypeTreeObject Get(
        UnityFileReader reader,
        MicroStack<TypeTreeNode> ancestors,
        long offset,
        TypeTreeNode node,
        SerializedFile sf)
    {
        try
        {
            var result = TypeTreeUtil.TryIntegralValue(reader, ancestors, offset, node)
                .OrElseWith(() => TypeTreeUtil.TryString(reader, ancestors, offset, node))
                .OrElseWith(() => TypeTreeUtil.TryArray(reader, ancestors, offset, node, sf))
                .OrElseWith(() =>
                {
                    if (node.IsManagedReferenceRegistry)
                    {
                        if (!node.IsLeaf && !ancestors.IsEmpty)
                        {
                            return Option.Some(new TypeTreeIgnored(node, ancestors, offset, node.SizeSafe()) as ITypeTreeObject);
                        }

                        throw new Exception($"{node.Type} not implemented");
                    }

                    return Option<ITypeTreeObject>.None;
                })
                .DefaultWith(() => TypeTreeUtil.GetObject(reader, ancestors, offset, node, sf));

            result =
                ObjectParsers.Parsers.Value.TryFind(p => p.CanParse(result.Node))
                    .Bind(p => p.TryParse(result, sf))
                    .DefaultValue(result);

            return result;
        }
        catch (Exception ex)
        {
            Debugger.Break();

            throw new Exception(
                $"Exception in node {node.Type} \"{node.Name}\" at offset {offset}:\n  {ex.Message}", ex);
        }
    }

    public static ITypeTreeObject Get(
        SerializedFile sf,
        UnityFileReader reader,
        ObjectInfo objectInfo) =>
        Get(reader, MicroStack<TypeTreeNode>.Empty, objectInfo.Offset, sf.GetTypeTreeRoot(objectInfo.Id), sf);

    public static IEnumerable<ITypeTreeObject> Find(this ITypeTreeObject obj, Func<ITypeTreeObject, bool> predicate)
    {
        if (predicate(obj))
            yield return obj;

        if (obj is TypeTreeValue<Dictionary<string, ITypeTreeObject>> tto)
        {
            foreach (var c in tto.Value.Values.SelectMany(c => c.Find(predicate)))
                yield return c;
        }
        else if (obj is TypeTreeValue<ITypeTreeObject[]> arr)
        {
            foreach (var c in arr.Value.SelectMany(c => c.Find(predicate)))
                yield return c;
        }
    }

    public static Option<Func<T?>> TryGetValue<T>(this ITypeTreeObject tto)
    {
        if (tto is TypeTreeValue<T?> obj)
            return Option.Some(() => obj.Value);

        return Option<Func<T?>>.None;
    }

    //public static Option<T[]> TryGetArray<T>(this ITypeTreeObject tto) => TryGetValue<T[]>(tto).Map(get => get());

    //public static Option<ITypeTreeObject[]> TryGetArray(this ITypeTreeObject tto) => TryGetArray<ITypeTreeObject>(tto);

    public static Option<TypeTreeValue<Dictionary<string, ITypeTreeObject>>> TryGetObject(this ITypeTreeObject tto)
    {
        if (tto is TypeTreeValue<Dictionary<string, ITypeTreeObject>> obj)
            return Option.Some(obj);
        
        return Option<TypeTreeValue<Dictionary<string, ITypeTreeObject>>>.None;
    }

    public static Option<Func<T?>> TryGetField<T>(this TypeTreeValue<Dictionary<string, ITypeTreeObject>> tto, string fieldName)
    {
        if (tto.Value.TryGetValue(fieldName, out var value))
            return value.TryGetValue<T?>();

        return Option<Func<T?>>.None;
    }

    //public static T? TryGetFieldValue<T>(this TypeTreeValue<Dictionary<string, ITypeTreeObject>> tto, string fieldName) =>
    //    tto.TryGetField<T>(fieldName).DefaultValue(() => default!)();
}
