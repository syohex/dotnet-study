let removeDuplicates (s: string) : string =
    let rec removeDuplicates' (cs: char list) (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            match acc with
            | [] -> removeDuplicates' t [ h ]
            | prev :: rest ->
                if h = prev then
                    removeDuplicates' t rest
                else
                    removeDuplicates' t (h :: acc)

    removeDuplicates' (s |> Seq.toList) []

// "ca"
removeDuplicates "abbaca"

// "ay"
removeDuplicates "azxxzy"

// "ab"
removeDuplicates "abbb"

// "a"
removeDuplicates "aaaaa"
