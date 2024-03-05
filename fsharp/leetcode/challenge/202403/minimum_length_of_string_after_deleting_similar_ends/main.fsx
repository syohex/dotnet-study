let minimumLength (s: string) : int =
    let rec minimumLength' left right (cs: char[]) =
        if left > right then
            0
        elif cs.[left] <> cs.[right] then
            right - left + 1
        else
            let c = cs.[left]

            let left' =
                seq { left..right }
                |> Seq.tryFind (fun n -> c <> cs.[n])
                |> Option.defaultValue (right + 1)

            let right' =
                seq { left..right }
                |> Seq.rev
                |> Seq.tryFind (fun n -> c <> cs.[n])
                |> Option.defaultValue (left - 1)

            minimumLength' left' right' cs

    minimumLength' 0 (s.Length - 1) (s |> Seq.toArray)

// 2
minimumLength "ca"

// 0
minimumLength "cabaabac"

// 3
minimumLength "aabccabba"

// 0
minimumLength "abbbbbbbbbbbbbbbbbbba"
