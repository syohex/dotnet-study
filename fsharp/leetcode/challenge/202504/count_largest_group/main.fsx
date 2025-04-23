let countLargestGroup (n: int) : int =
    let rec toDigitSum n acc =
        if n = 0 then acc else toDigitSum (n / 10) (acc + n % 10)

    let m =
        seq { 1..n }
        |> Seq.fold
            (fun acc n ->
                let sum = toDigitSum n 0
                let v = Map.tryFind sum acc |> Option.defaultValue 0
                Map.add sum (v + 1) acc)
            Map.empty

    let maxSize = m |> Map.values |> Seq.max
    m |> Map.values |> Seq.filter ((=) maxSize) |> Seq.length

// 4
countLargestGroup 13

// 2
countLargestGroup 2
