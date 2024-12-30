let countGoodStrings (low: int) (high: int) (zero: int) (one: int) : int =
    let modulo = 1_000_000_007L

    let rec countGoodStrings' i (dp: int64[]) =
        if i > high then
            seq { low..high } |> Seq.fold (fun acc i -> (acc + dp.[i]) % modulo) 0L |> int
        else
            dp.[i] <- if i >= zero then dp.[i - zero] else 0

            dp.[i] <-
                if i >= one then
                    (dp.[i] + dp.[i - one]) % modulo
                else
                    dp.[i]

            countGoodStrings' (i + 1) dp

    let dp: int64[] = Array.zeroCreate (high + 1)
    dp.[0] <- 1
    countGoodStrings' 1 dp

// 8
countGoodStrings 3 3 1 1

// 5
countGoodStrings 2 3 1 2

countGoodStrings 5000 10000 5 4
