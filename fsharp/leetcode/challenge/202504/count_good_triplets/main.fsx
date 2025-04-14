let countGoodTriplets (arr: int[]) (a: int) (b: int) (c: int) : int =
    let limit = arr.Length - 1

    seq {
        for i in 0 .. (limit - 2) do
            for j in (i + 1) .. (limit - 1) do
                let v1 = abs (arr.[i] - arr.[j])

                for k in (j + 1) .. limit do
                    let v2 = abs (arr.[j] - arr.[k])
                    let v3 = abs (arr.[i] - arr.[k])

                    if v1 <= a && v2 <= b && v3 <= c then
                        yield 1
    }
    |> Seq.length

// 4
countGoodTriplets [| 3; 0; 1; 1; 9; 7 |] 7 2 3

// 0
countGoodTriplets [| 1; 1; 2; 2; 3 |] 0 0 1
