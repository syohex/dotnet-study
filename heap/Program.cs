using System;

namespace heap
{
    class Heap
    {
        int[] Array { get; set; }
        int CurrentSize { get; set; }

        public Heap(int n)
        {
            Array = new int[n];
            CurrentSize = 0;
        }

        public static void Swap<T>(ref T lhs, ref T rhs)
        {
            T temp = lhs;
            lhs = rhs;
            rhs = temp;
        }

        public int Parent(int key)
        {
            return (key - 1) / 2;
        }

        public int Left(int key)
        {
            return 2 * key + 1;
        }

        public int Right(int key)
        {
            return 2 * key + 2;
        }

        public bool InsertKey(int key)
        {
            if (Array.Length == CurrentSize)
            {
                // heap is full
                return false;
            }

            int i = CurrentSize;
            Array[i] = key;
            CurrentSize++;

            while (i != 0 && Array[i] < Array[Parent(i)])
            {
                Swap(ref Array[i], ref Array[Parent(i)]);
                i = Parent(i);
            }

            return true;
        }

        public int GetMin()
        {
            return Array[0];
        }

        public int ExtractMin()
        {
            if (CurrentSize <= 0)
            {
                return int.MaxValue;
            }
            if (CurrentSize == 1)
            {
                CurrentSize--;
                return Array[0];
            }

            int root = Array[0];
            Array[0] = Array[CurrentSize - 1];
            CurrentSize--;

            Heapify(0);
            return root;
        }

        public void Heapify(int key)
        {
            int left = Left(key);
            int right = Right(key);

            int smallest = key;
            if (left < CurrentSize && Array[left] < Array[smallest])
            {
                smallest = left;
            }
            if (right < CurrentSize && Array[right] < Array[smallest])
            {
                smallest = right;
            }
            if (smallest != key)
            {
                Swap(ref Array[key], ref Array[smallest]);
                Heapify(smallest);
            }
        }

    }
    class Program
    {
        static void Main(string[] args)
        {
            var h = new Heap(11);
            h.InsertKey(9);
            h.InsertKey(3);
            h.InsertKey(15);
            h.InsertKey(20);
            h.InsertKey(21);
            h.InsertKey(45);

            for (var i = 0; i < 5; i++)
            {
                Console.WriteLine($"{i + 1} minimum val = {h.ExtractMin()}");
            }
        }
    }
}
