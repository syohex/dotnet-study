let countPartitions (nums: int list) : int =
    let rec countPartitions' nums left right acc =
        match nums with
        | [] -> failwith "never reach here"
        | _ :: [] -> acc
        | h :: t ->
            let left, right = left + h, right - h

            if (left - right) % 2 = 0 then
                countPartitions' t left right (acc + 1)
            else
                countPartitions' t left right acc

    let right = List.sum nums
    countPartitions' nums 0 right 0

// 4
countPartitions [ 10; 10; 3; 7; 6 ]

// 0
countPartitions [ 1; 2; 2 ]

// 3
countPartitions [ 2; 4; 6; 8 ]
