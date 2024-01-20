open System

let maxFrequencyElements (nums: int list) : int =
    let rec toFreq nums acc max =
        match nums with
        | [] -> acc, max
        | h :: t ->
            let v = Map.tryFind h acc |> Option.defaultValue 0
            toFreq t (Map.add h (v + 1) acc) (Math.Max(max, v + 1))

    let freq, max = toFreq nums Map.empty 0
    freq |> Map.values |> Seq.filter ((=) max) |> Seq.sum

// 4
maxFrequencyElements [ 1; 2; 2; 3; 1; 4 ]

// 5
maxFrequencyElements [ 1; 2; 3; 4; 5 ]
