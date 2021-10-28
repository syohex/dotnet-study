using System;
using System.Collections.Generic;

namespace EmptyListJoin
{
    class Program
    {
        static void Main(string[] args)
        {
            var list = new List<String> { "a", "b", "c" };
            var joint = string.Join(",", list);
            Console.WriteLine($"joint = {joint}");

            var empty_list = new List<String> { };
            var empty_joint = string.Join(",", empty_list);
            Console.WriteLine($"empty_joint = '{empty_joint}'");
        }
    }
}
