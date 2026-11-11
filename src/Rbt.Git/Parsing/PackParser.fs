namespace Rbt.Git

open System
open System.Buffers.Binary
open System.IO
open System.Security.Cryptography

/// Pack index parsing only. Object and delta decoding lives in PackData; repository
/// lookup and delta resolution live in PackStore.
module PackParser =
    let readPackIndex (path: string) : Result<PackIndex, string> =
        try
            let bytes = File.ReadAllBytes path
            // v2: magic, version, 256-entry fanout, object IDs, CRCs, offsets,
            // optional large offsets, pack checksum and index checksum.
            if bytes.Length < 8 + 256 * 4 + 40 then failwith "Truncated pack index"
            if bytes.[0..3] <> [| 0xffuy; 0x74uy; 0x4fuy; 0x63uy |] then
                failwith "Unsupported pack index signature"
            let read32 position = BinaryPrimitives.ReadUInt32BigEndian(bytes.AsSpan(position, 4))
            let read64 position = BinaryPrimitives.ReadUInt64BigEndian(bytes.AsSpan(position, 8))
            let version = int (read32 4)
            if version <> 2 then failwithf "Unsupported pack index version %d" version
            let fanout = Array.zeroCreate<int> 256
            let mutable previous = 0u
            for i in 0 .. 255 do
                let n = read32 (8 + i * 4)
                if n < previous || n > uint32 Int32.MaxValue then failwith "Invalid pack fanout"
                fanout.[i] <- int n
                previous <- n
            let count = int previous
            let fixedLength = 8L + 1024L + int64 count * 28L + 40L
            if fixedLength > int64 bytes.Length then failwith "Truncated pack index entries"
            let hashStart = 8 + 1024
            let crcStart = hashStart + 20 * count
            let offsetStart = crcStart + 4 * count
            let largeStart = offsetStart + 4 * count
            let mutable largeCount = 0
            for i in 0 .. count - 1 do
                if read32 (offsetStart + i * 4) &&& 0x80000000u <> 0u then
                    largeCount <- largeCount + 1
            if int64 bytes.Length <> fixedLength + int64 largeCount * 8L then
                failwith "Invalid pack large-offset table size"
            let expected = SHA1.HashData(bytes.AsSpan(0, bytes.Length - 20))
            if not (bytes.AsSpan(bytes.Length - 20, 20).SequenceEqual(expected)) then
                failwith "Pack index checksum mismatch"
            let objects = Array.zeroCreate<PackObjectEntry> count
            let mutable lastHash = ""
            for i in 0 .. count - 1 do
                let hash = Convert.ToHexString(bytes, hashStart + i * 20, 20).ToLowerInvariant()
                if i > 0 && String.CompareOrdinal(lastHash, hash) >= 0 then failwith "Unsorted pack index IDs"
                let first = int bytes.[hashStart + i * 20]
                if i >= fanout.[first] || (first > 0 && i < fanout.[first - 1]) then
                    failwith "Invalid pack fanout bucket"
                lastHash <- hash
                let offset = read32 (offsetStart + i * 4)
                let offset =
                    if offset &&& 0x80000000u = 0u then int64 offset
                    else
                        let largeIndex = int (offset &&& 0x7fffffffu)
                        if largeIndex >= largeCount then failwith "Invalid pack large-offset index"
                        let big = read64 (largeStart + largeIndex * 8)
                        if big > uint64 Int64.MaxValue then failwith "Pack offset overflow"
                        int64 big
                if offset < 12L then failwith "Invalid pack object offset"
                objects.[i] <- { Hash = hash; Offset = offset; CRC = read32 (crcStart + i * 4) }
            Ok { Version = version; Fanout = fanout; Objects = objects }
        with ex -> Error $"Failed to read pack index: {ex.Message}"
