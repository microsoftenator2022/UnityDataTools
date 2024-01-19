namespace UnityMicro.Parsers;

using System.Text.RegularExpressions;

using MicroUtils;

using UnityDataTools.FileSystem;

using UnityMicro.TypeTree;

public readonly partial record struct PPtr(string TypeName, int FileID, long PathID)
{
    [GeneratedRegex(@"^PPtr<(\w+)>$")]
    internal static partial Regex PPtrPattern();

    public static Option<PPtr> FromTypeTree(ITypeTreeObject obj)
    {
        var match = PPtrPattern().Match(obj.NodeType());

        if (!match.Success)
            return Option<PPtr>.None;

        var typeName = match.Groups[1].Value;

        var p = obj.TryGetObject();
        var fid = p.Bind(p => p.TryGetField<int>("m_FileID")).Map(f => f());
        var pid = p.Bind(p => p.TryGetField<long>("m_PathID")).Map(f => f());

        if (fid.IsSome && pid.IsSome)
            return Option.Some(new PPtr(typeName, fid.Value, pid.Value));

        return Option<PPtr>.None;
    }
}

class PPtrParser : IObjectParser
{
    public bool CanParse(TypeTreeNode node) => PPtr.PPtrPattern().IsMatch(node.Type);
    public Type ObjectType(TypeTreeNode node) => typeof(PPtr);
    public Option<ITypeTreeObject> TryParse(ITypeTreeObject obj) => PPtr.FromTypeTree(obj).Map<PPtr, ITypeTreeObject>(pptr =>
        new TypeTreeValue<PPtr>(obj.Node, obj.Ancestors, obj.StartOffset, obj.EndOffset, pptr));
}
