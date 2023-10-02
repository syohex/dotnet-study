let winnerOfGame (colors: string) : bool =
    seq { 1 .. (colors.Length - 2) }
    |> Seq.fold
        (fun (a, b) i ->
            if colors.[i - 1] = colors.[i] && colors.[i] = colors.[i + 1] then
                if colors.[i] = 'A' then a + 1, b else a, b + 1
            else
                a, b)
        (0, 0)
    ||> (>)

// true
winnerOfGame "AAABABB"

// false
winnerOfGame "AA"

// false
winnerOfGame "ABBBBBBBAAA"
