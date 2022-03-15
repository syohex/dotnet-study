open System
open System.IO

let removeEmptyLines (input: string) : string list =
    input
    |> File.ReadLines
    |> Seq.filter (fun s -> s |> String.IsNullOrEmpty |> not)
    |> Seq.toList
    
removeEmptyLines "problem34.fsx"