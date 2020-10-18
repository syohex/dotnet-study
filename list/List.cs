using System;

namespace list
{
    internal class List
    {
        internal int Value { get; }
        internal List? Next { get; private set; }

        internal List(int val)
        {
            Value = val;
            Next = null;
        }

        internal void Insert(int index, int val)
        {
            if (index == 0)
            {
                var tmp = Next;
                Next = new List(val);
                Next.Next = tmp;
                return;
            }

            var p = Next;
            List? prev = null;
            for (var i = 0; i < index; i++)
            {
                if (p == null)
                {
                    return;
                }

                prev = p;
                p = p.Next;
            }

            if (prev != null && p != null)
            {
                prev.Next = new List(val);
                prev.Next.Next = p;
            }
        }

        internal void InsertHead(int val)
        {
            Insert(0, val);
        }

        internal void InsertTail(int val)
        {
            var p = Next;
            if (p == null)
            {
                Next = new List(val);
                return;
            }

            while (p.Next != null)
            {
                p = p.Next;
            }

            p.Next = new List(val);
        }

        internal void Print()
        {
            Console.Write("[");
            var p = Next;
            while (p != null)
            {
                Console.Write($" {p.Value} ");
                p = p.Next;
            }
            Console.WriteLine("]");
        }
    }
}
