let numSub (s: string) : int =
    let modulo = 1_000_000_007L

    let sum (n: int64) : int64 = ((n * (1L + n)) / 2L) % modulo

    let rec numSub' cs ones acc =
        match cs with
        | [] -> acc + sum ones |> int
        | h :: t ->
            if h = '1' then
                numSub' t (ones + 1L) acc
            else
                numSub' t 0 (acc + sum ones)

    numSub' (Seq.toList s) 0 0

// 9
numSub "0110111"

// 2
numSub "101"

// 21
numSub "111111"
