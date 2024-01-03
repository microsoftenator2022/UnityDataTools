using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AssetTool.Util
{
    public record class MicroStack<T>(T Head, MicroStack<T> Tail) : IEnumerable<T>
    {
        public MicroStack(T Head) : this(Head, Empty) { }

        public static readonly MicroStack<T> Empty = new(default!, null!);

        public bool IsEmpty => this == Empty;

        public MicroStack<T> Push(T value) => new(value, this);
        public (T, MicroStack<T>) Pop() =>
            this.IsEmpty ?
            throw new InvalidOperationException("Cannot pop from an empty stack") :
            (this.Head, this.Tail);

        public void Deconstruct(out T head, out MicroStack<T> tail) => (head, tail) = this.Pop();

        public IEnumerable<T> Enumerable
        {
            get
            {
                if (this.IsEmpty)
                    yield break;

                yield return this.Head;

                foreach (var value in Tail)
                    yield return value;
            }
        }

        public IEnumerator<T> GetEnumerator() => this.Enumerable.GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => this.Enumerable.GetEnumerator();

        public static MicroStack<T> OfEnumerable(IEnumerable<T> source)
        {
            var stack = Empty;
            foreach (var item in source.Reverse())
                stack = (item, stack);

            return stack;
        }

        //public static MicroStack<T> operator +(T value, MicroStack<T> stack) => stack.Push(value);

        public static implicit operator MicroStack<T>((T head, MicroStack<T> tail) cons) => new(cons.head, cons.tail);
    }
}
