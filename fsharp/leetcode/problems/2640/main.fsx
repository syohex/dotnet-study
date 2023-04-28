let findPrefixScore (nums: int list) : int64 list =
    let rec findPrefixScore' (nums: int64 list) max sum acc =
        match nums with
        | [] -> List.rev acc
        | h :: t ->
            let max' = System.Math.Max(h, max)
            let sum' = sum + max' + h
            findPrefixScore' t max' sum' (sum' :: acc)

    let nums' = nums |> List.map int64
    findPrefixScore' nums' 0 0 []

// [4;10;24;36;56]
findPrefixScore [ 2; 3; 7; 5; 10 ]

// [2;4;8;16;32;64]
findPrefixScore [ 1; 1; 2; 4; 8; 16 ]
