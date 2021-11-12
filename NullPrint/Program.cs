int? a = null;
string? b = null;

// If value is null, nothing display
Console.WriteLine($"a='{a}'");
Console.WriteLine($"b='{b}'");

// condition expression cannot be used in string interpolation
if (b == null) {
    Console.WriteLine("b = null");
} else {
    Console.WriteLine($"b = {b}");
}

