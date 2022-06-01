let runningSum (nums: int list) =
    let rec runningSum' nums sum acc =
        match nums with
        | [] -> acc |> List.rev
        | h :: t ->
            let sum' = sum + h
            runningSum' t sum' (sum' :: acc)

    runningSum' nums 0 []

// [1;3;6;10]
runningSum [ 1; 2; 3; 4 ]

// [1;2;3;4;5]
runningSum [ 1; 1; 1; 1; 1 ]

// [3;4;6;16;17]
runningSum [ 3; 1; 2; 10; 1 ]
