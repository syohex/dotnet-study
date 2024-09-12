let countConsistentStrings (allowed: string) (words: string list) : int =
    let allowed =
        allowed
        |> Seq.fold
            (fun (acc: bool[]) c ->
                acc.[int c - int 'a'] <- true
                acc)
            (Array.zeroCreate 26)

    let isConsistent word =
        word |> Seq.forall (fun c -> allowed.[int c - int 'a'])

    words |> List.filter isConsistent |> List.length

// 2
countConsistentStrings "ab" [ "ad"; "bd"; "aaab"; "baa"; "badab" ]

// 7
countConsistentStrings "abc" [ "a"; "b"; "c"; "ab"; "ac"; "bc"; "abc" ]

// 4
countConsistentStrings "cad" [ "cc"; "acd"; "b"; "ba"; "bac"; "bad"; "ac"; "d" ]
