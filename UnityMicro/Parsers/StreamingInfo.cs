namespace UnityMicro.Parsers;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using MicroUtils.Functional;

using UnityDataTools.FileSystem;

using UnityMicro.TypeTree;

public readonly record struct StreamingInfo(ulong Offset, uint Size, string RawPath)
{
    public byte[] GetData()
    {
        var match = StreamingInfoParser.PathRegex().Match(RawPath);
        var mountPoint = match.Groups["MountPoint"].Value;
        var archive = match.Groups["ParentPath"].Value;
        var path = match.Groups["ResourcePath"].Value;

        return [];
    }
}

partial class StreamingInfoParser : IObjectParser
{
    [GeneratedRegex(@"^(?'MountPoint'.+?)[\\\/](?:(?'ParentPath'.+?)[\\\/])*(?'ResourcePath'.+)$")]
    internal static partial Regex PathRegex();
    public bool CanParse(TypeTreeNode node) => node.Type == "StreamingInfo";
    public Type ObjectType(TypeTreeNode _) => typeof(StreamingInfo);
    public Option<ITypeTreeObject> TryParse(ITypeTreeObject obj, SerializedFile sf)
    {
        if (!CanParse(obj.Node))
            return Option<ITypeTreeObject>.None;

        var si = obj.TryGetObject();
        var path = si
            .Bind(si => si.TryGetField<string?>("path"))
            .Bind(path => 
            {
                var s = path();
                if (string.IsNullOrEmpty(s))
                    return Option<string>.None;

                return Option.Some(s);
            });

        var offset = si.Bind(si => si.TryGetField<ulong>("offset")).Map(offset => offset());
        var size = si.Bind(si => si.TryGetField<uint>("size")).Map(size => size());

        if (path.IsNone || offset.IsNone || size.IsNone)
            return Option<ITypeTreeObject>.None;

        var node = obj.Node;

        return Option.Some<ITypeTreeObject>(new TypeTreeValue<StreamingInfo>(
            node,
            obj.Ancestors,
            obj.StartOffset,
            obj.EndOffset,
            new(offset.Value, size.Value, path.Value)));
    }
}

