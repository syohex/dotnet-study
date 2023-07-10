using System.Collections.Concurrent;

ConcurrentBag<int> a = new();
a.Add(1);
a.Add(2);
a.Add(3);
a.Add(4);
a.Add(5);
a.Add(6);

while (a.TryTake(out var b)) {
    Console.WriteLine($"Take value={b}");
}
