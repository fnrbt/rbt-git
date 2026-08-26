namespace Rbt.Git

open System
open System.IO
open System.Text

/// pkt-line framing (git wire protocol). A packet is a 4-hex-digit big-endian
/// length (counting the 4 length bytes themselves) followed by the payload.
/// Special packets: "0000" = flush, "0001" = delimiter (protocol v2).
module PktLine =

    type Pkt =
        | Flush
        | Delim
        | Data of byte[]

    let flushBytes = Encoding.ASCII.GetBytes "0000"
    let delimBytes = Encoding.ASCII.GetBytes "0001"

    /// Frame a payload as a pkt-line.
    let encode (payload: byte[]) : byte[] =
        let len = payload.Length + 4
        if len > 65520 then failwith "pkt-line payload too large"
        let prefix = Encoding.ASCII.GetBytes(len.ToString("x4"))
        Array.append prefix payload

    let encodeStr (s: string) : byte[] =
        encode (Encoding.UTF8.GetBytes s)

    /// Read the pkt-lines from a buffer starting at `pos`. Returns the packet and
    /// the new position. Stops the caller via Flush/Delim sentinels.
    let read (buf: byte[]) (pos: int) : Pkt * int =
        if pos + 4 > buf.Length then Flush, buf.Length
        else
            let lenHex = Encoding.ASCII.GetString(buf, pos, 4)
            let len = Convert.ToInt32(lenHex, 16)
            if len = 0 then Flush, pos + 4
            elif len = 1 then Delim, pos + 4
            else
                let payloadLen = len - 4
                let payload = buf.[pos + 4 .. pos + 4 + payloadLen - 1]
                Data payload, pos + len

    /// Parse all pkt-lines from `pos` until a flush packet (or end of buffer).
    /// Returns the data payloads and the position just past the flush.
    let readUntilFlush (buf: byte[]) (pos: int) : byte[] list * int =
        let rec loop p acc =
            if p >= buf.Length then List.rev acc, p
            else
                match read buf p with
                | Flush, np -> List.rev acc, np
                | Delim, np -> loop np acc
                | Data d, np -> loop np (d :: acc)
        loop pos []

    /// Read one pkt-line from a stream. None at a clean end of stream.
    let readFrom (s: Stream) : Pkt option =
        let hdr = Array.zeroCreate 4
        let mutable off = 0
        let mutable eof = false
        while off < 4 && not eof do
            let r = s.Read(hdr, off, 4 - off)
            if r = 0 then eof <- true else off <- off + r
        if off = 0 then None
        elif off < 4 then None
        else
            let len = Convert.ToInt32(Encoding.ASCII.GetString hdr, 16)
            if len = 0 then Some Flush
            elif len = 1 then Some Delim
            else
                let payloadLen = len - 4
                let payload = Array.zeroCreate payloadLen
                let mutable p = 0
                while p < payloadLen do
                    let r = s.Read(payload, p, payloadLen - p)
                    if r = 0 then failwith "truncated pkt-line"
                    p <- p + r
                Some(Data payload)

    /// Convenience: write a flush packet to a stream.
    let writeFlush (s: Stream) = s.Write(flushBytes, 0, 4)

    /// Convenience: write a delimiter packet (0001, protocol v2 section separator).
    let writeDelim (s: Stream) = s.Write(delimBytes, 0, 4)

    /// Convenience: write a framed string to a stream.
    let writeStr (s: Stream) (str: string) =
        let b = encodeStr str
        s.Write(b, 0, b.Length)
