let vowelStrings (words: string list) (queries: (int * int) list) : int list =
    let isVowel c =
        match c with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> true
        | _ -> false

    let acc =
        words
        |> List.fold
            (fun (acc, prev) w ->
                let s, e = Seq.head w, Seq.last w

                if isVowel s && isVowel e then
                    (prev + 1) :: acc, prev + 1
                else
                    prev :: acc, prev)
            ([ 0 ], 0)
        |> fst
        |> List.rev
        |> List.toArray

    queries
    |> List.fold (fun ret (l, r) -> acc.[r + 1] - acc.[l] :: ret) []
    |> List.rev

// [2,3,0]
vowelStrings [ "aba"; "bcb"; "ece"; "aa"; "e" ] [ (0, 2); (1, 4); (1, 1) ]
// [3,2,1]
vowelStrings [ "a"; "e"; "i" ] [ (0, 2); (0, 1); (2, 2) ]
