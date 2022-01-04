let checkString (s: string) : bool =
    let cs = s |> Seq.toList
    let lastA = cs |> List.tryFindIndexBack (fun c -> c = 'a')
    let firstB = cs |> List.tryFindIndexBack (fun c -> c = 'b')

    match (lastA, firstB) with
    | (None, None) -> true
    | (None, Some _) -> true
    | (Some _, None) -> true
    | (Some a, Some b) -> a < b

checkString "aaa"
checkString "aaabbb"
checkString "ababa"
checkString "bbb"
checkString "bbbaaa"