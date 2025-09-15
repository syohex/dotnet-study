let canBeTypedWords (text: string) (brokenLetters: string) : int =
    let broken = brokenLetters |> Set.ofSeq

    text.Split [| ' ' |]
    |> Array.fold
        (fun acc w ->
            if Seq.exists (fun c -> Set.contains c broken) w then
                acc
            else
                acc + 1)
        0

// 1
canBeTypedWords "hello world" "ad"

// 1
canBeTypedWords "leet code" "lt"

// 0
canBeTypedWords "leet code" "e"
