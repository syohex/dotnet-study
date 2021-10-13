using System;
using System.Collections.Generic;
using System.Text.Json;

namespace JsonSerialize01
{
    class Program
    {
        static void Main(string[] args)
        {
            var dict = new Dictionary<string, object> {
                {"foo", "bar"},
                {"baz", 20},
                {"age", true},
                {"hoge", null},
            };

            var json = JsonSerializer.Serialize(dict);
            Console.WriteLine(json);
        }
    }
}
