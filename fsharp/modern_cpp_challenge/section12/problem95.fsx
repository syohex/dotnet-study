open System.Net
open System.Net.Sockets

// problem 95
let solveHostName (host: string) : string list =
    try
        let entry = Dns.GetHostEntry(host)

        entry.AddressList
        |> Array.filter (fun a -> a.AddressFamily = AddressFamily.InterNetwork)
        |> Array.map (fun a -> a.ToString())
        |> Array.toList
    with
    | e -> failwith (sprintf "exception: %s" e.Message)

solveHostName "syohex.org"
solveHostName "google.com"
