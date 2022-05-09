let letterCombinations (digits: string) : string list =
    let table =
        [| [ 'a'; 'b'; 'c' ]
           [ 'd'; 'e'; 'f' ]
           [ 'g'; 'h'; 'i' ]
           [ 'j'; 'k'; 'l' ]
           [ 'm'; 'n'; 'o' ]
           [ 'p'; 'q'; 's'; 'r' ]
           [ 't'; 'u'; 'v' ]
           [ 'w'; 'x'; 'y'; 'z' ] |]

    let rec letterCombinations' indexes (acc: char list) =
        match indexes with
        | [] ->
            let s = acc |> List.rev |> System.String.Concat
            [ s ]
        | h :: t ->
            table.[h]
            |> List.fold
                (fun v c ->
                    let ret = letterCombinations' t (c :: acc)
                    ret @ v)
                []


    if System.String.IsNullOrEmpty digits then
        []
    else
        let indexes =
            digits
            |> Seq.map (fun c -> int c - int '0' - 2)
            |> Seq.toList

        letterCombinations' indexes [] |> List.rev

//  ["ad","ae","af","bd","be","bf","cd","ce","cf"]
letterCombinations "23"

// []
letterCombinations ""

// ["a","b","c"]
letterCombinations "2"
