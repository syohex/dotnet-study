let getLucky (s: string) (k: int) : int =
    let rec getLucky' i cs acc =
        if i >= k then
            acc
        else
            match cs with
            | [] -> failwith "never reach here"
            | _ :: [] -> acc
            | _ ->
                let acc = cs |> List.map (fun c -> int c - int '0') |> List.sum
                getLucky' (i + 1) (acc |> string |> Seq.toList) acc

    let cs =
        s
        |> Seq.map (fun c -> int c - int 'a' + 1)
        |> System.String.Concat
        |> Seq.toList

    getLucky' 0 cs 0

// 36
getLucky "iiii" 1

// 6
getLucky "leetcode" 2

// 8
getLucky "zbax" 2
