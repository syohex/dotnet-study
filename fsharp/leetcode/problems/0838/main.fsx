let pushDominoes (dominoes: string) : string =
    let len = dominoes.Length

    let (values, _, _) =
        dominoes
        |> Seq.fold
            (fun ((acc: int []), idx, value) c ->
                match c with
                | 'L' ->
                    acc.[idx] <- 0
                    acc, idx + 1, 0
                | 'R' ->
                    acc.[idx] <- len
                    acc, idx + 1, len
                | _ ->
                    let v = System.Math.Max(0, value - 1)
                    acc.[idx] <- v
                    acc, idx + 1, v)
            (Array.zeroCreate len, 0, 0)

    let (values', _, _) =
        dominoes
        |> Seq.rev
        |> Seq.fold
            (fun ((acc: int []), idx, value) c ->
                match c with
                | 'L' ->
                    acc.[idx] <- acc.[idx] - len
                    acc, idx - 1, len
                | 'R' -> acc, idx - 1, 0
                | _ ->
                    let v = System.Math.Max(0, value - 1)
                    acc.[idx] <- acc.[idx] - v
                    acc, idx - 1, v)
            (values, len - 1, 0)

    values'
    |> Array.map (fun v ->
        if v > 0 then 'R'
        elif v = 0 then '.'
        else 'L')
    |> System.String

// "RR.L"
pushDominoes "RR.L"

// "LL.RR.LLRRLL.."
pushDominoes ".L.R...LR..L.."
