using UnityDataTools.FileSystem;

namespace AssetTool
{
    using System.Diagnostics;

    using Util;

    internal class Program
    {
        const int blockSize = 1024 * 1024 * 1024;

        static void PrintNodes(string mountPoint, string filePath)
        {
            //var objectOffsets = new List<(string, long, long)>();

            var archive = UnityFileSystem.MountArchive(filePath, mountPoint);

            Console.WriteLine($"{archive.Nodes.Count} nodes");

            foreach (var node in archive.Nodes)
            {
                var nodePath = $"{mountPoint}{node.Path}";

                Console.WriteLine($" {nodePath}");
                Console.WriteLine($"  Size: {node.Size}");
                Console.WriteLine($"  Flags: {node.Flags}");

                if (node.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
                {
                    using var sf = UnityFileSystem.OpenSerializedFile(nodePath);
                    using var reader = new UnityFileReader(nodePath, blockSize);

                    Console.WriteLine($"  {sf.Objects.Count} objects:");

                    foreach (var o in sf.Objects)
                    {
                        var root = sf.GetTypeTreeRoot(o.Id);
                        //try
                        //{
                            var objectData = TypeTreeObject.Get(reader, MicroStack<TypeTreeNode>.Empty, o.Offset, root);

                            if (objectData is TypeTreeValue<Dictionary<string, ITypeTreeObject>> obj)
                            {
                                var name = "";

                                if (obj.Value.TryGetValue("m_Name", out var nameObject))
                                {
                                    if (nameObject is TypeTreeValue<string> s)
                                        name = s.Value;
                                }

                            //Console.WriteLine($"   {root.Type} \"{name}\"");

                            //Console.WriteLine($"     {obj.StartOffset}-{obj.EndOffset}");

                            //objectOffsets.Add(($"{name}_{root.Type}", obj.StartOffset, obj.EndOffset));
                        }
                        //}
                        //catch (Exception ex)
                        //{
                        //    Console.WriteLine(ex);
                        //    throw;
                        //}
                    }

                    //if (!Directory.Exists("offsets"))
                    //{
                    //    Directory.CreateDirectory("offsets");
                    //}

                    //File.WriteAllText($"offsets\\{node.Path}.offsets.txt",
                    //    String.Concat(objectOffsets
                    //        .Select(offsets => $"{offsets.Item1}:{offsets.Item2},{offsets.Item3}")
                    //        .SelectMany(s => new string[] { s, "\n" })
                    //        .ToArray()));

                    //objectOffsets.Clear();
                }
            }

            archive.Dispose();
        }

        static void Main(string[] args)
        {
            var (path, fileName) = args.Length > 1 ? (args[0], args[1]) : (Path.GetDirectoryName(args[0]), Path.GetFileName(args[0]));

            UnityFileSystem.Init();

            var mountPoint = $"archive:{Path.DirectorySeparatorChar}";

            var filePath = Path.Join(path, fileName);

            PrintNodes(mountPoint, filePath);

            UnityFileSystem.Cleanup();
        }
    }
}
