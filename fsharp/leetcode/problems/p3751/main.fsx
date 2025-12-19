let totalWaviness (num1: int) (num2: int) : int =
    let f n =
        string n
        |> Seq.windowed 3
        |> Seq.fold
            (fun acc v ->
                if (v.[0] < v.[1] && v.[1] > v.[2]) || (v.[0] > v.[1] && v.[1] < v.[2]) then
                    acc + 1
                else
                    acc)
            0


    seq { num1..num2 } |> Seq.fold (fun acc n -> acc + f n) 0

// 3
totalWaviness 120 130

// 3
totalWaviness 198 202

// 2
totalWaviness 4848 4848
