open System
open System.Net.Http

let getContent url =
    async {
        use client = new HttpClient()
        use handler = new HttpClientHandler()

        client.BaseAddress <- url
        handler.UseCookies <- false

        let message =
            new HttpRequestMessage(HttpMethod.Get, "/")

        message.Headers.Add("Cookie", "key=value")

        let! response = client.SendAsync(message) |> Async.AwaitTask

        return!
            response.Content.ReadAsStringAsync()
            |> Async.AwaitTask
    }

[<EntryPoint>]
let main args : int =
    let content =
        getContent (new Uri("https://syohex.org"))
        |> Async.RunSynchronously

    printfn $"{content}"
    0
