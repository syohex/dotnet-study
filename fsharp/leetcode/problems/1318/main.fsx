let minFlips (a: int) (b: int) (c: int) =
    let rec minFlips' i mask a b c acc =
        if i = 0 then
            acc
        else
            let i' = i - 1
            let mask' = mask <<< 1
            let a' = a &&& mask <> 0
            let b' = b &&& mask <> 0

            match c &&& mask <> 0 with
            | true ->
                match a', b' with
                | false, false -> minFlips' i' mask' a b c (acc + 1)
                | _ -> minFlips' i' mask' a b c acc
            | false ->
                match a', b' with
                | true, false
                | false, true -> minFlips' i' mask' a b c (acc + 1)
                | true, true -> minFlips' i' mask' a b c (acc + 2)
                | _ -> minFlips' i' mask' a b c acc

    minFlips' 32 1 a b c 0

// 3
minFlips 2 6 5

// 1
minFlips 4 2 7

// 0
minFlips 1 2 3
