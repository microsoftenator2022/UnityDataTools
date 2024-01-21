using System;
using System.Buffers;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
//using Force.Crc32;

namespace UnityDataTools.FileSystem;

// This class can be used to read typed data from a UnityFile. Is uses a buffer for better performance.
public class UnityFileReader : IDisposable
{
    UnityFile   m_File;
    byte[]      m_Buffer;
    long        m_BufferStartInFile;
    long        m_BufferEndInFile;

    readonly ArrayPool<byte> arrayPool = ArrayPool<byte>.Create();
    readonly int bufferStartSize;
    readonly int maxBufferSize;
    public long Length { get; }

    public UnityFileReader(string path, int bufferSize, int maxBufferSize = 1024 * 1024 * 1024)
    {
        bufferStartSize = bufferSize;
        this.maxBufferSize = maxBufferSize;

        m_Buffer = arrayPool.Rent(bufferStartSize);
        m_BufferStartInFile = 0;
        m_BufferEndInFile = -1;

        m_File = UnityFileSystem.OpenFile(path);
        Length = m_File.GetSize();
    }

    static bool IsSubRangeOf(long rangeStart, long rangeLength, long subRangeStart, long subRangeLength) =>
        subRangeStart >= rangeStart && (subRangeStart + subRangeLength) <= (rangeStart + rangeLength);

    static bool IsSubRangeOf((long start, long length) range, (long start, long length) subRange) =>
        IsSubRangeOf(range.start, range.length, subRange.start, subRange.length);

    int GetBufferOffset(long fileOffset, int count)
    {
        //// Should we update the buffer ?
        //if (fileOffset < m_BufferStartInFile || fileOffset + count > m_BufferEndInFile)
        //{
        //    if (count > m_Buffer.Length)
        //        throw new IOException("Requested size is larger than cache size");

        //    m_BufferStartInFile = m_File.Seek(fileOffset);

        //    if (m_BufferStartInFile != fileOffset)
        //        throw new IOException("Invalid file offset");

        //    m_BufferEndInFile = m_File.Read(m_Buffer.Length, m_Buffer);
        //    m_BufferEndInFile += m_BufferStartInFile;
        //}

        //return (int)(fileOffset - m_BufferStartInFile);

        var requestedRange = (fileOffset, count);
        
        var updateBuffer = false;
        long bufferLength = m_Buffer.Length;
        long bufferOffset = m_BufferStartInFile;

        var dataLength = (m_BufferEndInFile - m_BufferStartInFile + 1);

        if (dataLength < bufferLength || !IsSubRangeOf((bufferOffset, bufferLength), requestedRange))
        {
            updateBuffer = true;
        }

        if (updateBuffer)
        {
            if (IsSubRangeOf((bufferOffset, bufferLength * 2), requestedRange))
            {
                bufferLength *= 2;
            }
            else if (IsSubRangeOf((bufferOffset - bufferLength, bufferLength * 2), requestedRange))
            {
                bufferOffset = Math.Max(bufferOffset - bufferLength, 0);
                bufferLength *= 2;
            }
            
            bufferLength = Math.Min(bufferLength, maxBufferSize);

            if (!IsSubRangeOf((bufferOffset, bufferLength), requestedRange))
            {
                (bufferOffset, bufferLength) = requestedRange;
                bufferLength = Math.Max(bufferLength, bufferStartSize);
            }

            if (count > bufferLength)
                throw new IOException("Requested size is larger than cache size");

            var oldBuffer = m_Buffer;

            if (bufferLength != m_Buffer.Length)
            {
                // Get the new buffer here
                m_Buffer = arrayPool.Rent((int)bufferLength);
            }

            //if (m_BufferStartInFile != bufferOffset)
            //    Console.WriteLine($"Update buffer offset: {m_BufferStartInFile} -> {bufferOffset}");

            //if (oldBuffer.Length != m_Buffer.Length)
            //    Console.WriteLine($"Update buffer size: {oldBuffer.Length} -> {m_Buffer.Length}");

            m_BufferStartInFile = m_File.Seek(bufferOffset);

            if (m_BufferStartInFile > fileOffset || m_BufferStartInFile + bufferLength < fileOffset + count)
            {
                Debugger.Break();
                throw new IOException("Invalid file offset. " +
                    $"Requested {fileOffset} -> {fileOffset + count}, " +
                    $"buffer {m_BufferStartInFile} -> {m_BufferStartInFile + bufferLength}");
            }

            var readCount = m_File.Read(bufferLength, m_Buffer);
            m_BufferEndInFile = m_BufferStartInFile + readCount;

            // Don't release the old buffer until here. Seems like the UnityFileSystemApi library expects to be able to
            // use the old buffer for the *next* read (cache for overlapping reads?)
            arrayPool.Return(oldBuffer);
        }

        return (int)(fileOffset - m_BufferStartInFile);
    }

    public void ReadArray(long fileOffset, int size, Array dest)
    {
        var offset = GetBufferOffset(fileOffset, size);
        Buffer.BlockCopy(m_Buffer, offset, dest, 0, size);
    }
        
    public string ReadString(long fileOffset, int size)
    {
        var offset = GetBufferOffset(fileOffset, size);
        return Encoding.Default.GetString(m_Buffer, offset, size);
    }

    public float ReadFloat(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 4);
        return BitConverter.ToSingle(m_Buffer, offset);
    }

    public double ReadDouble(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 8);
        return BitConverter.ToDouble(m_Buffer, offset);
    }

    public long ReadInt64(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 8);
        return BitConverter.ToInt64(m_Buffer, offset);
    }

    public ulong ReadUInt64(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 8);
        return BitConverter.ToUInt64(m_Buffer, offset);
    }

    public int ReadInt32(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 4);
        return BitConverter.ToInt32(m_Buffer, offset);
    }

    public uint ReadUInt32(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 4);
        return BitConverter.ToUInt32(m_Buffer, offset);
    }

    public short ReadInt16(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 2);
        return BitConverter.ToInt16(m_Buffer, offset);
    }

    public ushort ReadUInt16(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 2);
        return BitConverter.ToUInt16(m_Buffer, offset);
    }

    public sbyte ReadInt8(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 1);
        return (sbyte)m_Buffer[offset];
    }

    public byte ReadUInt8(long fileOffset)
    {
        var offset = GetBufferOffset(fileOffset, 1);
        return m_Buffer[offset];
    }

    //public uint ComputeCRC(long fileOffset, int size, uint crc32 = 0)
    //{
    //    var readSize = size > m_Buffer.Length ? m_Buffer.Length : size;
    //    var readBytes = 0;
        
    //    while (readBytes < size)
    //    {
    //        var offset = GetBufferOffset(fileOffset, readSize);
    //        crc32 = Crc32Algorithm.Append(crc32, m_Buffer, offset, readSize);
    //        readBytes += readSize;
    //    }

    //    return crc32;
    //}

    public void Dispose()
    {
        m_File.Dispose();
    }
}