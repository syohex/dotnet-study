let maxAbsoluteSum (nums: int list) : int =
    let maxVal, _ =
        nums
        |> List.fold
            (fun (acc, sum) n ->
                let sum = max (sum + n) n
                max acc sum, sum)
            (0, 0)

    let minVal, _ =
        nums
        |> List.fold
            (fun (acc, sum) n ->
                let sum = min (sum + n) n
                min acc sum, sum)
            (0, 0)

    max (System.Math.Abs(maxVal)) (System.Math.Abs(minVal))

// 5
maxAbsoluteSum [ 1; -3; 2; 3; -4 ]

// 8
maxAbsoluteSum [ 2; -5; 1; -4; 3; 2 ]
