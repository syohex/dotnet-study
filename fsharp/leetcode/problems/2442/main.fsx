let countDistinctIntegers (nums: int list) : int =
    let rec reverse n acc =
        if n = 0 then
            acc
        else
            reverse (n / 10) (acc * 10 + (n % 10))

    nums
    |> List.fold (fun acc num -> Set.add num acc |> Set.add (reverse num 0)) Set.empty
    |> Set.toList
    |> List.length

// 6
countDistinctIntegers [ 1
                        13
                        10
                        12
                        31 ]

// 1
countDistinctIntegers [ 2; 2; 2 ]
