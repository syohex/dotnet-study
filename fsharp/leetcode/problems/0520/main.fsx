open System

let detectCapitalUse (word: string) : bool =
    let cs = word |> Seq.toList

    match cs with
    | [] -> true
    | c :: tail when Char.IsLower(c) -> tail |> List.forall Char.IsLower
    | _ :: tail ->
        (tail |> List.forall Char.IsLower)
        || (tail |> List.forall Char.IsUpper)

// true
detectCapitalUse "USA"
// true
detectCapitalUse "leetcode"
// true
detectCapitalUse "Google"
// true
detectCapitalUse "USA"
// false
detectCapitalUse "FlaG"
// true
detectCapitalUse "ggg"
// true
detectCapitalUse "g"
// false
detectCapitalUse "FFFFFFFFFFFFFFf"
