using System;
using System.Threading;

namespace Sample
{
    class MyThread
    {
        public static void Do(object n)
        {
            var k = (int)n;
            for (int i = 1; i <= 10; i++)
            {
                Console.WriteLine($"n + 1 = {k + i}");
                Thread.Sleep(500);
            }
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            ParameterizedThreadStart p = MyThread.Do;
            Thread t = new Thread(p);
            t.Start(10);
        }
    }
}
