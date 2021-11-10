var input = new List<string>
{
    "12345",
    "1e40",
    "1E40",
    "1E307",
    "3.141592653",
};

foreach (var s in input)
{
    if (Double.TryParse(s, out double value))
    {
        Console.WriteLine($"TryParse({s}) ok: {value}");
    }
    else
    {
        Console.WriteLine($"TryParse({s}) failed: {value}");
    }
}
