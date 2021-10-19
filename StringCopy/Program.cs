using System;

namespace StringCopy
{
    class Program
    {
        static void Main(string[] args)
        {
            string a = "hello world";
            string b = a;

            Console.WriteLine($"a={a}({a.GetHashCode()}), b={b}({b.GetHashCode()})");
            a += "k";
            Console.WriteLine($"a={a}({a.GetHashCode()}), b={b}({b.GetHashCode()})");
        }
    }
}
