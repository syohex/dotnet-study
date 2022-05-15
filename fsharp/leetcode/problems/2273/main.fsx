let removeAnagrams (words: string list) : string list =
    let rec removeAnagrams' (words: string list) (prev: int []) acc =
        match words with
        | [] -> acc |> List.rev
        | h :: t ->
            let freq =
                h
                |> Seq.fold
                    (fun (acc: int []) c ->
                        let index = (int c) - (int 'a')
                        acc.[index] <- acc.[index] + 1
                        acc)
                    (Array.zeroCreate 26)

            match Array.compareWith (fun a b -> compare a b) freq prev with
            | 0 -> removeAnagrams' t prev acc
            | _ -> removeAnagrams' t freq (h :: acc)


    removeAnagrams' words (Array.zeroCreate 26) []

// ["abba", "cd"]
removeAnagrams [ "abba"
                 "baba"
                 "bbaa"
                 "cd"
                 "cd" ]

// ["a", "b", "c", "d", "e"]
removeAnagrams [ "a"
                 "b"
                 "c"
                 "d"
                 "e" ]

// ["a", "b", "a"]
removeAnagrams [ "a"; "b"; "a" ]
