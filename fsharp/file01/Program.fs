open System.IO
// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main args =
    let lines = args |> Array.map File.ReadAllLines

    let contents =
        lines
        |> Array.map (fun ls -> System.String.Join("\n", ls))

    contents |> Array.iter (fun a -> printfn "%s\n" a)
    0
