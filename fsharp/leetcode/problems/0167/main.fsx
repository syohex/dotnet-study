let twoSum (numbers: int []) (target: int) : (int * int) =
    let rec twoSum' left right (numbers: int []) target =
        if left >= right then
            failwith "never reach here"
        else
            let sum = numbers.[left] + numbers.[right]

            if sum = target then
                (left + 1, right + 1)
            elif sum < target then
                twoSum' (left + 1) right numbers target
            else
                twoSum' left (right - 1) numbers target

    twoSum' 0 (numbers.Length - 1) numbers target

// (1, 2)
twoSum [| 2; 7; 11; 15 |] 9

// (1;3)
twoSum [| 2; 3; 4 |] 6

// (-1;0)
twoSum [| -1; 0 |] -1
