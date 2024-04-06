open System

let minRemoveToMakeValid (s: string) : string =
    let rec invalidParentheses cs q =
        match cs with
        | [] -> q |> List.map fst |> List.rev |> Set.ofList
        | (i, h) :: t ->
            match h with
            | '(' -> invalidParentheses t ((i, h) :: q)
            | ')' ->
                match q with
                | [] -> invalidParentheses t ((i, h) :: q)
                | (_, h') :: t' ->
                    if h' = '(' then
                        invalidParentheses t t'
                    else
                        invalidParentheses t ((i, h) :: q)
            | _ -> invalidParentheses t q

    let cs = Seq.toList s |> List.indexed
    let invalidIndexes = invalidParentheses cs []

    s
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> not <| Set.contains i invalidIndexes)
    |> Seq.map snd
    |> String.Concat

// lee(t(c)o)de
minRemoveToMakeValid "lee(t(c)o)de)"

// "ab(c)d"
minRemoveToMakeValid "a)b(c)d"

// ""
minRemoveToMakeValid "))(("

// "abc"
minRemoveToMakeValid "abc"
