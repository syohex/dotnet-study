class TaskCompletionSourceDemo
{
    static async Task Main()
    {
        TaskCompletionSource<int> tcs1 = new TaskCompletionSource<int>();
        Task<int> t1 = tcs1.Task;

        // Start a background task that will complete tcs1.Task
        var _ = Task.Factory.StartNew(() =>
        {
            Console.WriteLine("## Before Sleep");
            Thread.Sleep(1000);
            Console.WriteLine("## After Sleep");
            tcs1.SetResult(15);
        });

        Console.WriteLine("## AWAIT");
        int result = await t1;
        Console.WriteLine($"result={result}");
    }
}