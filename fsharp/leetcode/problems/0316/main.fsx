let stringToFreqencies (s: string) : Map<char, int> =
    let rec stringToFreqencies' cs m =
        match cs with
        | [] -> m
        | h :: t ->
            match Map.tryFind h m with
            | Some (n) -> stringToFreqencies' t (Map.add h (n + 1) m)
            | None -> stringToFreqencies' t (Map.add h 1 m)

    stringToFreqencies' (s |> Seq.toList) Map.empty

let removeDuplicateLetters (s: string) : string =
    let rec removeLargerCharacters c stack freq seen =
        match stack with
        | [] -> [], seen
        | h :: t ->
            match Map.tryFind h freq with
            | Some (n) ->
                if n > 0 && h > c then
                    let seen = Set.remove h seen
                    removeLargerCharacters c t freq seen
                else
                    stack, seen
            | None -> failwith "never reach here"

    let rec removeDuplicateLetters' cs freq seen (stack: char list) =
        match cs with
        | [] -> stack |> List.rev |> System.String.Concat
        | h :: t ->
            if Set.contains h seen then
                removeDuplicateLetters' t freq seen stack
            else
                let freq =
                    match Map.tryFind h freq with
                    | Some (n) -> Map.add h (n - 1) freq
                    | None -> failwith "never reach here"

                let (stack, seen) = removeLargerCharacters h stack freq seen
                removeDuplicateLetters' t freq (Set.add h seen) (h :: stack)

    let freq = stringToFreqencies s
    removeDuplicateLetters' (s |> Seq.toList) freq Set.empty []

// "abc"
removeDuplicateLetters "bcabc"

// "cbacdcbc"
removeDuplicateLetters "acdb"
