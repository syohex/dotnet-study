using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;

namespace SystemCommandLine
{
    class Program
    {
        static int Main(string[] args)
        {
            var rootCommand = new RootCommand
            {
                new Option<int>(
                    "--int-option",
                    getDefaultValue: () => 42,
                    description: "An option whose argument is parsed as an int"
                ),
                new Option<bool>(
                    "--bool-option",
                    "An option whose argument is parsed as a bool"
                ),
                new Option<FileInfo>(
                    "--file-option",
                    "An option whose argument is parsed as a FileInfo"
                )
            };

            rootCommand.Handler = CommandHandler.Create<int, bool, FileInfo>((intOption, boolOption, fileOption) =>
            {
                Console.WriteLine($"--int-option is: {intOption}");
                Console.WriteLine($"--bool-option is: {boolOption}");
                Console.WriteLine($"--file-option is: {fileOption?.FullName ?? null}");
            });

            return rootCommand.InvokeAsync(args).Result;
        }
    }
}
