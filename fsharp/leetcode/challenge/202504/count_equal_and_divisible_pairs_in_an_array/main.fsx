let countPairs (nums: int[]) (k: int) : int =
    let len = nums.Length

    seq {
        for i in 0 .. (len - 2) do
            for j in (i + 1) .. (len - 1) do
                if nums.[i] = nums.[j] && (i * j) % k = 0 then
                    yield 1
    }
    |> Seq.length

// 4
countPairs [| 3; 1; 2; 2; 2; 1; 3 |] 2

// 0
countPairs [| 1; 2; 3; 4 |] 1
