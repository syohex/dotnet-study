let makeGood (s: string) : string =
    let rec makeGood' cs (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            match acc with
            | [] -> makeGood' t (h :: acc)
            | prev :: rest ->
                if (System.Char.IsLower(h)
                    && prev = System.Char.ToUpper(h))
                   || (System.Char.IsUpper(h)
                       && prev = System.Char.ToLower(h)) then
                    makeGood' t rest
                else
                    makeGood' t (h :: acc)

    makeGood' (s |> Seq.toList) []

// leetcode
makeGood "leEeetcode"

// ""
makeGood "abBAcC"

// ""
makeGood "Pp"

// ""
makeGood "pP"

// "aa"
makeGood "aa"
