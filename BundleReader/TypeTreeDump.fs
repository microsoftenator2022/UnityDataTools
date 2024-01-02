module rec UnityData.TypeTreeDump

open UnityData.TypeTree

let private lines (s : string) = s.Split(System.Environment.NewLine.ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)

let dumpObject (this : TypeTreeObject) =
    seq {
        yield $"Node '{this.Node.Name}'"
        yield $"  Type: {this.Node.Type} ({this.Node.CSharpType})"
        yield $"  Offset: {this.StartOffset}"
        yield $"  Node size: {this.Node.Size}"
        yield $"  Data size: {this.EndOffset - this.StartOffset}"
        yield $"  Next node: {this.NextNodeOffset}"
        yield $"  Value:"
        
        let (s : string) = this.Value |> valueToString

        for line in s |> lines do
            yield $"    {line}"
    }
    |> String.concat (System.Environment.NewLine)

let valueToString (this : TypeTreeValue) =
    match this with
    | TypeTreeValue.SInt32 v -> v.ToString()
    | TypeTreeValue.UInt32 v -> v.ToString()
    | TypeTreeValue.Float v -> v.ToString()
    | TypeTreeValue.Double v -> v.ToString()
    | TypeTreeValue.SInt16 v -> v.ToString()
    | TypeTreeValue.UInt16 v -> v.ToString()
    | TypeTreeValue.SInt64 v -> v.ToString()
    | TypeTreeValue.UInt64 v -> v.ToString()
    | TypeTreeValue.SInt8 v -> v.ToString()
    | TypeTreeValue.UInt8 v -> v.ToString()
    | TypeTreeValue.Bool v -> v.ToString()
    | TypeTreeValue.String v -> v.ToString()
    | TypeTreeValue.IntegralArray v -> v.ToString()
    | TypeTreeValue.Array v -> v.ToString()
    | TypeTreeValue.Object v -> v.ToString()
    | TypeTreeValue.Skipped -> ""

let dumpValue this =
    match this with
    | TypeTreeValue.SInt32 v -> $"{nameof(TypeTreeValue.SInt32)} {v}"
    | TypeTreeValue.UInt32 v -> $"{nameof(TypeTreeValue.UInt32)} {v}"
    | TypeTreeValue.Float v -> $"{nameof(TypeTreeValue.Float)} {v}"
    | TypeTreeValue.Double v -> $"{nameof(TypeTreeValue.Double)} {v}"
    | TypeTreeValue.SInt16 v -> $"{nameof(TypeTreeValue.SInt16)} {v}"
    | TypeTreeValue.UInt16 v -> $"{nameof(TypeTreeValue.UInt16)} {v}"
    | TypeTreeValue.SInt64 v -> $"{nameof(TypeTreeValue.SInt64)} {v}"
    | TypeTreeValue.UInt64 v -> $"{nameof(TypeTreeValue.SInt64)} {v}"
    | TypeTreeValue.SInt8 v -> $"{nameof(TypeTreeValue.SInt8)} {v}"
    | TypeTreeValue.UInt8 v -> $"{nameof(TypeTreeValue.UInt8)} {v}"
    | TypeTreeValue.Bool v -> $"{nameof(TypeTreeValue.Bool)} {v}"
    | TypeTreeValue.Skipped -> $"{nameof(TypeTreeValue.Skipped)}"
    | TypeTreeValue.String v -> $"{nameof(TypeTreeValue.String)} \"{v}\""
    | TypeTreeValue.IntegralArray v ->
        "[|"
        // + (seq { for element in v do yield $" {valueToString element}" } |> String.concat ";")
        + $"{v}"
        + " |]"

    | TypeTreeValue.Array v ->
        seq {
            yield "[|"
            for element in v do
                for line in dumpObject element |> lines do
                    yield $"  {line}"
                yield ";"
            yield "|]"
        }
        |> String.concat (System.Environment.NewLine)
        
    | TypeTreeValue.Object v ->
        seq {
            yield "{"
            for child in v.Properties do
                yield $"  '{child.Key}':"

                for line in child.Value |> dumpObject |> lines do
                    yield $"    {line}"
            yield "}"
        }
        |> String.concat (System.Environment.NewLine)
