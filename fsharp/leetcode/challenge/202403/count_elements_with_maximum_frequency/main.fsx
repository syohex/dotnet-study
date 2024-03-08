let maxFrequencyElements (nums: int list) : int =
    let max, freq =
        nums
        |> List.fold
            (fun (max, freq) n ->
                let v = Map.tryFind n freq |> Option.defaultValue 0
                System.Math.Max(max, v + 1), Map.add n (v + 1) freq)
            (0, Map.empty)

    freq |> Map.fold (fun acc _ v -> if v = max then acc + v else acc) 0

// 4
maxFrequencyElements [ 1; 2; 2; 3; 1; 4 ]

// 5
maxFrequencyElements [ 1; 2; 3; 4; 5 ]
