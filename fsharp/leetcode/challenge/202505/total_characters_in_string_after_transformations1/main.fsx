let lengthAfterTransformations (s: string) (t: int) : int =
    let modulo = 1_000_000_007

    let rec lengthAfterTransformations' i (freq: int[]) =
        if i >= t then
            freq |> Array.fold (fun acc n -> (acc + n) % modulo) 0
        else
            let a, b = freq.[25], (freq.[0] + freq.[25]) % modulo
            let rest = freq |> Array.skip 1 |> Array.take 24
            lengthAfterTransformations' (i + 1) (Array.concat [ [| a; b |]; rest ])

    let freq =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    lengthAfterTransformations' 0 freq

// 7
lengthAfterTransformations "abcyy" 2

// 5
lengthAfterTransformations "azbk" 1
