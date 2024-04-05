open System

let makeGood (s: string) : string =
    let rec makeGood' (cs: char list) (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> String.Concat
        | h :: t ->
            match acc with
            | [] -> makeGood' t (h :: acc)
            | h' :: t' ->
                if h = h' then makeGood' t (h :: acc)
                elif Char.ToLower(h) = Char.ToLower(h') then makeGood' t t'
                else makeGood' t (h :: acc)

    makeGood' (Seq.toList s) []

// "leetcode"
makeGood "leEeetcode"

// ""
makeGood "abBAcC"

// "s"
makeGood "s"

// "Pp"
makeGood ""
