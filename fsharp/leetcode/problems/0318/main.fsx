open System

let maxProduct (words: string list) : int =
    let toBitMask (s: string) =
        s
        |> Seq.fold
            (fun acc c ->
                let v = 1 <<< (int c - int 'a')
                acc ||| v)
            0

    let rec maxProduct' (ws: (string * int) list) acc =
        match ws with
        | [] -> acc
        | (word, mask) :: rest ->
            let acc' =
                rest
                |> List.filter (fun (_, m) -> (mask &&& m) = 0)
                |> List.fold (fun acc (w, _) -> Math.Max(acc, word.Length * w.Length)) acc

            maxProduct' rest acc'

    let masks = words |> List.map toBitMask
    let ws = List.zip words masks
    maxProduct' ws 0

// 16
maxProduct [ "abcw"
             "baz"
             "foo"
             "bar"
             "xtfn"
             "abcdef" ]

// 4
maxProduct [ "a"
             "ab"
             "abc"
             "d"
             "cd"
             "bcd"
             "abcd" ]

// 0
maxProduct [ "a"; "aa"; "aaa"; "aaaa" ]
