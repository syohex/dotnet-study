using System;

namespace StringTrim01
{
    class Program
    {
        static void Main(string[] args)
        {
            string text = "https://syohex.org/";
            string stripped = text.Replace("https://", "").Trim('/');
            Console.WriteLine(stripped);

            text = "???foo***";
            Console.WriteLine(text.TrimStart('?').TrimEnd('*'));
        }
    }
}
