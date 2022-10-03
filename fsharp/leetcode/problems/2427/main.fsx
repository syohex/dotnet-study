let commonFactors (a: int) (b: int) : int =
    seq { 1 .. (System.Math.Min(a, b)) }
    |> Seq.fold
        (fun acc i ->
            if a % i = 0 && b % i = 0 then
                acc + 1
            else
                acc)
        0

// 4
commonFactors 12 6

// 2
commonFactors 25 30
