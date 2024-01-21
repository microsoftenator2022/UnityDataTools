namespace UnityDataTools.FileSystem;

using System;
using System.Collections.Generic;
using System.Text;

public struct ExternalReference
{
    public string Path;
    public string Guid;
    public ExternalReferenceType Type;
}

public class SerializedFile : IDisposable
{
    List<ExternalReference> m_ExternalReferences;
    ObjectInfo[] m_Objects;

    Dictionary<IntPtr, TypeTreeNode> m_TypeTreeCache = new Dictionary<IntPtr, TypeTreeNode>();

    internal SerializedFileHandle m_Handle;

    public IReadOnlyList<ExternalReference> ExternalReferences => m_ExternalReferences.AsReadOnly();
    public IReadOnlyList<ObjectInfo> Objects => Array.AsReadOnly(m_Objects);

    private readonly Dictionary<long, int> idToIndex = [];
    public ObjectInfo GetObjectByID(long pathID)
    {
        if (!idToIndex.ContainsKey(pathID))
        {
            for (var i = 0; i < m_Objects.Length; i++)
            {
                if (m_Objects[i].Id == pathID)
                {
                    idToIndex[pathID] = i;
                    break;
                }
            }
        }

        return m_Objects[idToIndex[pathID]];
    }

    public readonly string Path;

    internal SerializedFile(SerializedFileHandle handle, string path)
    {
        m_Handle = handle;
        m_ExternalReferences = GetExternalReferences();
        m_Objects = GetObjects();

        Path = path;
    }

    public TypeTreeNode GetTypeTreeRoot(long objectId)
    {
        var r = DllWrapper.GetTypeTree(m_Handle, objectId, out var typeTreeHandle);
        UnityFileSystem.HandleErrors(r);

        if (m_TypeTreeCache.TryGetValue(typeTreeHandle.Handle, out var node))
        {
            return node;
        }

        node = new TypeTreeNode(typeTreeHandle, 0);
        m_TypeTreeCache.Add(typeTreeHandle.Handle, node);

        return node;
    }

    public TypeTreeNode GetRefTypeTypeTreeRoot(string className, string namespaceName, string assemblyName)
    {
        var r = DllWrapper.GetRefTypeTypeTree(m_Handle, className, namespaceName, assemblyName, out var typeTreeHandle);
        UnityFileSystem.HandleErrors(r);

        if (m_TypeTreeCache.TryGetValue(typeTreeHandle.Handle, out var node))
        {
            return node;
        }

        node = new TypeTreeNode(typeTreeHandle, 0);
        m_TypeTreeCache.Add(typeTreeHandle.Handle, node);

        return node;
    }

    private List<ExternalReference> GetExternalReferences()
    {
        var r = DllWrapper.GetExternalReferenceCount(m_Handle, out var count);
        UnityFileSystem.HandleErrors(r);

        var externalReferences = new List<ExternalReference>(count);
        var path = new StringBuilder(512);
        var guid = new StringBuilder(32);

        for (var i = 0; i < count; ++i)
        {
            DllWrapper.GetExternalReference(m_Handle, i, path, path.Capacity, guid, out var externalReferenceType);
            UnityFileSystem.HandleErrors(r);

            externalReferences.Add(new ExternalReference() { Path = path.ToString(), Guid = guid.ToString(), Type = externalReferenceType });
        }

        return externalReferences;
    }

    private ObjectInfo[] GetObjects()
    {
        var r = DllWrapper.GetObjectCount(m_Handle, out var count);
        UnityFileSystem.HandleErrors(r);

        if (count == 0)
            return [];

        var objs = new ObjectInfo[count];
        DllWrapper.GetObjectInfo(m_Handle, objs, count);
        UnityFileSystem.HandleErrors(r);

        return objs;
    }

    public void Dispose()
    {
        if (m_Handle != null && !m_Handle.IsInvalid)
        {
            m_Handle.Dispose();
        }
    }
}