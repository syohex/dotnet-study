open System

let detectCapitalUse (word: string) : bool =
    let cs = word |> Seq.toList

    if (cs |> List.head |> Char.IsLower) then
        cs |> List.forall Char.IsLower
    else
        cs |> List.forall Char.IsUpper
        || cs |> List.skip 1 |> List.forall Char.IsLower

// true
detectCapitalUse "USE"

// true
detectCapitalUse "Google"

// true
detectCapitalUse "g"

// false
detectCapitalUse "FlaG"
