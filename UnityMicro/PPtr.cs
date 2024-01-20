namespace UnityMicro.Parsers;

using System.Text.RegularExpressions;

using MicroUtils;
using MicroUtils.Functional;

using UnityDataTools.FileSystem;

using UnityMicro.TypeTree;

public static class PPtrExtensions
{
    public static Option<ITypeTreeObject> TryGet(this PPtr pptr, SerializedFile sf, UnityFileReader reader)
    {
        try
        {
            // Invalid/null pointer
            if (pptr.PathID == 0)
                return Option<ITypeTreeObject>.None;

            if (pptr.FileID != 0)
                // TODO
                return Option<ITypeTreeObject>.None;

            return Option.Some(TypeTreeObject.Get(sf, reader, sf.GetObjectByID(pptr.PathID)));
        }
        catch (KeyNotFoundException)
        {
            return Option<ITypeTreeObject>.None;
        }
    }
}

public readonly partial record struct PPtr(string TypeName, int FileID, long PathID);

partial class PPtrParser : IObjectParser
{
    [GeneratedRegex(@"^PPtr<(\w+)>$")]
    internal static partial Regex PPtrPattern();

    public bool CanParse(TypeTreeNode node) => PPtrPattern().IsMatch(node.Type);
    public Type ObjectType(TypeTreeNode node) => typeof(PPtr);
    public Option<ITypeTreeObject> TryParse(ITypeTreeObject obj)
    {
        var match = PPtrPattern().Match(obj.NodeType());

        if (!match.Success)
            return Option<ITypeTreeObject>.None;

        var typeName = match.Groups[1].Value;

        var p = obj.TryGetObject();
        var fid = p.Bind(p => p.TryGetField<int>("m_FileID")).Map(f => f());
        var pid = p.Bind(p => p.TryGetField<long>("m_PathID")).Map(f => f());

        if (fid.IsSome && pid.IsSome)
            return Option.Some<ITypeTreeObject>(new TypeTreeValue<PPtr>(
                obj.Node,
                obj.Ancestors,
                obj.StartOffset,
                obj.EndOffset,
                new PPtr(typeName, fid.Value, pid.Value)));

        return Option<ITypeTreeObject>.None;
    }
}
