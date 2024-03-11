open System

let customSortString (order: string) (s: string) : string =
    let table =
        order
        |> Seq.map (fun c -> int c - int 'a')
        |> Seq.indexed
        |> Seq.fold
            (fun (acc: int[]) (i, n) ->
                acc.[n] <- i
                acc)
            (Array.init 26 (fun _ -> 27))

    s |> Seq.sortBy (fun c -> table.[int c - int 'a']) |> String.Concat

// "cbad"
customSortString "cba" "abcd"

// "bcad"
customSortString "bcafg" "abcd"
