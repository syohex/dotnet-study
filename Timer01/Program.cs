using System;

public class Program
{
    public static void Main(string[] args)
    {
        SynchronizationContext context = SynchronizationContext.Current;
        System.Timers.Timer timer = new System.Timers.Timer(5000);
        timer.Elapsed += (sender, e) =>
        {
            Console.WriteLine($"timer handler: {context}");
            context.Send(state =>
            {
                Console.WriteLine("hello timer");
                throw new Exception("uwaaa");
            }, null);
            throw new Exception("uweee");
        };

        timer.Start();
        Console.WriteLine("start timer");

        Thread.Sleep(10000);
        Console.WriteLine("finish");
    }
}