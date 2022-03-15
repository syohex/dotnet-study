let invalidParentheses (s: string) : int list =
    let rec invalidParentheses' cs acc =
        match cs with
        | [] -> acc |> List.map fst |> List.rev
        | (idx, c) :: tail ->
            match c with
            | '(' -> invalidParentheses' tail ((idx, c) :: acc)
            | ')' ->
                match acc with
                | (_, top) :: restStack when top = '(' -> invalidParentheses' tail restStack
                | _ -> invalidParentheses' tail ((idx, c) :: acc)
            | _ -> invalidParentheses' tail acc

    invalidParentheses' (s |> Seq.mapi (fun i c -> (i, c)) |> Seq.toList) []

let minRemoveToMakeValid (s: string) : string =
    let rec minRemoveToMakeValid' (cs: (int * char) list) idxs (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | (idx, h1) :: t1 ->
            match idxs with
            | h2 :: t2 when idx = h2 -> minRemoveToMakeValid' t1 t2 acc
            | _ -> minRemoveToMakeValid' t1 idxs (h1 :: acc)

    let invalidIndexs = invalidParentheses s
    minRemoveToMakeValid' (s |> Seq.mapi (fun i c -> i, c) |> Seq.toList) invalidIndexs []

// "lee(t(c)o)de"
minRemoveToMakeValid "lee(t(c)o)de)"

// "ab(c)d"
minRemoveToMakeValid "a)b(c)d"

// ""
minRemoveToMakeValid "))(("
