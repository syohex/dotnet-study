var now = DateTimeOffset.Now;
var from = DateTimeOffset.FromUnixTimeSeconds(1647253699);

if (from < now) {
    Console.WriteLine("It is not expired");
} else {
    Console.WriteLine("expired");
}
