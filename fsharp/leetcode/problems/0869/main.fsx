let reorderedPowerOf2 (n: int) : bool =
    let rec toDigitsStr n acc =
        if n = 0 then
            acc |> List.sort |> string
        else
            toDigitsStr (n / 10) ((n % 10) :: acc)
    
    let s = toDigitsStr n []
    seq { 0..30 }
    |> Seq.map (fun m -> System.Math.Pow(2, m) |> int)
    |> Seq.map (fun m -> toDigitsStr m [])
    |> Seq.tryFind (fun t -> s = t)
    |> Option.isSome

// true
reorderedPowerOf2 1

// false
reorderedPowerOf2 10

// true
reorderedPowerOf2 55366