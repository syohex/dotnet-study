await TestFunc();

static async Task TestFunc()
{
    using (var dispose = new TestAsync())
    {
        Console.WriteLine("Before RunAsync");
        await dispose.RunAsync();
        Console.WriteLine("After RunAsync");
    }
}

public class TestAsync : IDisposable
{
    public void Dispose()
    {
        Console.WriteLine("Dispose");
    }

    public async Task RunAsync()
    {
        Console.WriteLine("RunAsync");
        await Task.Delay(2000);
    }
}
