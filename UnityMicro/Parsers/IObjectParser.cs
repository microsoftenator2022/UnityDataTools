namespace UnityMicro.Parsers;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

using MicroUtils.Functional;

using UnityDataTools.FileSystem;

using UnityMicro.TypeTree;

public interface IObjectParser
{
    bool CanParse(TypeTreeNode node);
    Type ObjectType(TypeTreeNode node);
    Option<ITypeTreeObject> TryParse(ITypeTreeObject obj, SerializedFile sf);
}

public static class ObjectParsers
{
    public static IEnumerable<IObjectParser> FromAssembly(Assembly assembly) =>
        assembly.GetTypes()
            .Where(t =>
                !t.IsInterface &&
                !t.IsAbstract &&
                t.GetInterfaces().Any(t => typeof(IObjectParser).IsAssignableFrom(t)) &&
                t.GetConstructor(Type.EmptyTypes) is not null)
            .Select(Activator.CreateInstance)
            .Cast<IObjectParser>();

    public static readonly Lazy<ImmutableArray<IObjectParser>> Parsers =
        new(() => ObjectParsers.FromAssembly(Assembly.GetExecutingAssembly()).ToImmutableArray());
}
