let minimizedStringLength (s: string) : int =
    s |> Set.ofSeq |> Set.toList |> List.length

// 3
minimizedStringLength "aaabc"

// 3
minimizedStringLength "cbbd"

// 2
minimizedStringLength "dddaaa"
