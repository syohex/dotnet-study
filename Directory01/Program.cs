using System;
using System.IO;

namespace Directory01
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 2)
            {
                Console.Error.WriteLine("Usage: dotnet run directory");
                return;
            }

            string dir = args[0];
            string pattern = args[1];
            string[] dirs = Directory.GetDirectories(dir, pattern, SearchOption.AllDirectories);
            if (dirs.Length == 0)
            {
                Console.WriteLine($"no directories which matches against {pattern} into {dir}");
                return;
            }

            for (var i = 1; i <= dirs.Length; i++)
            {
                Console.WriteLine($"[{i}] {dirs[i - 1]}");
            }
        }
    }
}
