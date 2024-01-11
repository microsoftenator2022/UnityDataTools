module UnityFS.Interop.FSharp.Wrappers

open UnityFS.Interop
open System.Collections

type NativeResult<'a> = Result<'a, ReturnCode>

let handleError (returnCode : ReturnCode, result) : NativeResult<_> =
    if returnCode = ReturnCode.Success then
        Ok result
    else
        Error returnCode

type CSharpTypeTreeHandle = TypeTreeHandle

[<CustomEquality; CustomComparison>]
type TypeTreeHandle = { Handle : CSharpTypeTreeHandle; SerializedFile : SerializedFileHandle; ObjectId : int64 }
with
    member this.Comparable = this.Handle.Handle, this.SerializedFile.Handle, this.ObjectId
    override this.Equals(other) =
        match other with
        | :? TypeTreeHandle as other ->
            this.Comparable.Equals(other.Comparable)
        | _ -> false
    override this.GetHashCode() = hash (this.Handle.Handle, this.SerializedFile.Handle, this.ObjectId)
    interface IStructuralComparable with
        member this.CompareTo(other, comparer) =
            match other with
            | :? TypeTreeHandle as other ->
                if this = other then 0
                else comparer.Compare(this.Comparable, other.Comparable)
            | _ -> raise (System.ArgumentException())
                