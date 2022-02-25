open System
open System.Net.Http

let downloadURL (url: Uri) =
    async {
        let client = new HttpClient()
        let! content = client.GetStringAsync(url) |> Async.AwaitTask
        return content
    }

[<EntryPoint>]
let main args =
    let url =
        if args.Length >= 1 then
            args.[0]
        else
            Console.ReadLine()

    let content =
        downloadURL (new Uri(url))
        |> Async.RunSynchronously

    printfn $"{content}"
    0
