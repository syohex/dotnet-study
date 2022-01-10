let capitalizeTitle (s: string) : string =
    let f (s: string) : string =
        if s.Length <= 2 then
            s.ToLower()
        else
            let first = s |> Seq.head |> System.Char.ToUpper

            let rest =
                s
                |> Seq.tail
                |> Seq.map System.Char.ToLower
                |> Seq.toList

            (first :: rest) |> System.String.Concat

    let ss = s.Split([| ' ' |]) |> Array.map f
    System.String.Join(' ', ss)

capitalizeTitle "capiTalIze tHe title"
capitalizeTitle "First leTTER OF EACH WORd"
capitalizeTitle "i lOve leetcode"
