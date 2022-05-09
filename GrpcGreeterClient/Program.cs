using System.Threading.Tasks;
using Grpc.Net.Client;
using GrpcGreeterClient;

using var channel = GrpcChannel.ForAddress("https://localhost:7054");
var client = new Greeter.GreeterClient(channel);
var reply = await client.SayHelloAsync(
    new HelloRequest { Name = "GreeterClient" }
);

Console.WriteLine($"Greeting: {reply.Message}");
Console.WriteLine("Press any key to exit...");
Console.ReadLine();