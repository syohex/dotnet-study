let countAsterisks (s: string) : int =
    s
    |> Seq.fold
        (fun (ret, inBars) c ->
            if c = '|' then
                ret, inBars |> not
            elif c = '*' && (not inBars) then
                ret + 1, inBars
            else
                ret, inBars)
        (0, false)
    |> fst

// 2
countAsterisks "l|*e*et|c**o|*de|"

// 0
countAsterisks "iamprogrammer"

// 5
countAsterisks "yo|uar|e**|b|e***au|tifu|l"
