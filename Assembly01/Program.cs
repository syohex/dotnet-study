using System.Reflection;

var file = args[0];
Console.WriteLine($"file={file}");

var ass = Assembly.LoadFrom(file);
Console.WriteLine($"runtime version: {ass.ImageRuntimeVersion}");