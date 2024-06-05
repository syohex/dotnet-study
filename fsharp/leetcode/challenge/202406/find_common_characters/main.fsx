let commonChars (words: string list) : string list =
    let toFreq s =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    words
    |> List.map toFreq
    |> List.reduce (fun acc v ->
        seq { 0..25 } |> Seq.iter (fun i -> acc.[i] <- min acc.[i] v[i])
        acc)
    |> Array.indexed
    |> Array.fold
        (fun acc (i, n) ->
            let c = i + int 'a' |> char |> string

            if n > 0 then
                seq { 0 .. (n - 1) } |> Seq.fold (fun a _ -> c :: a) acc
            else
                acc)
        []
    |> List.rev

// ["e";"l";"l"]
commonChars [ "bella"; "label"; "roller" ]

// ["c";"o"]
commonChars [ "cool"; "lock"; "cook" ]
