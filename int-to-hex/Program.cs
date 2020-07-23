using System;

namespace IntToHex
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Enter Decimal Number > ");
            Console.Out.Flush();
            
            var input = Console.ReadLine();
            input = input.Replace(System.Environment.NewLine, "");

            int number = 0;
            try {
                number = Int32.Parse(input);
            } catch (FormatException) {
                Console.WriteLine($"Can't parse {input} as decimal number");
                System.Environment.Exit(1);
            }

            char[] buf = new char[input.Length + 2];
            var i = 0;
            while (true) {
                var mod = number % 16;
                number = number / 16;

                char c;
                if (mod >= 0 && mod <= 9) {
                    c = (char)(mod + '0');
                } else {
                    c = (char)(mod - 10 + 'A');
                }
                buf[i++] = c;

                if (number == 0) {
                    break;
                }
            }

            buf[i++] = 'x';
            buf[i++] = '0';

            Array.Reverse(buf);
            var hex = new string(buf);
            Console.WriteLine($"Decimal {input}, Hex -> {hex}");
        }
    }
}
