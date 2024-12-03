open System

let addSpaces (s: string) (spaces: int list) : string =
    let rec addSpaces' cs spaces (acc: char list) =
        match cs, spaces with
        | [], [] -> acc |> List.rev |> String.Concat
        | [], _ -> failwith "never reach here"
        | (_, h) :: t, [] -> addSpaces' t [] (h :: acc)
        | (i, c) :: t1, j :: t2 ->
            if i = j then
                addSpaces' t1 t2 (c :: ' ' :: acc)
            else
                addSpaces' t1 spaces (c :: acc)

    let cs = s |> Seq.indexed |> Seq.toList
    addSpaces' cs spaces []

// "Leetcode Helps Me Learn"
addSpaces "LeetcodeHelpsMeLearn" [ 8; 13; 15 ]

// "i code in py thon"
addSpaces "icodeinpython" [ 1; 5; 7; 9 ]

// " s p a c i n g"
addSpaces "spacing" [ 0..6 ]
