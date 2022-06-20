open System

let minimumLengthEncoding (words: string list) : int =
    let rec substrs (cs: char list) acc =
        match cs with
        | [] -> acc
        | _ :: t ->
            let str = cs |> String.Concat
            substrs t (str :: acc)

    let rec removeSubstrs (words: string list) s =
        match words with
        | [] -> s
        | h :: t ->
            let strs = substrs (h |> Seq.toList |> List.tail) []

            let s' =
                strs
                |> List.fold (fun acc s -> Set.remove s acc) s

            removeSubstrs t s'

    let s = words |> Set.ofList
    let s' = removeSubstrs words s
    s' |> Set.fold (fun acc s -> acc + s.Length + 1) 0

// 10
minimumLengthEncoding [ "time"
                        "me"
                        "bell" ]

// 2
minimumLengthEncoding [ "t" ]

// 5
minimumLengthEncoding [ "time"
                        "time"
                        "time"
                        "time" ]
