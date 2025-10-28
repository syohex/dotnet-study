let countValidSelections (nums: int list) : int =
    let rec countValidSelections' nums leftSum sum acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let leftSum = leftSum + h

            if h <> 0 then
                countValidSelections' t leftSum sum acc
            else
                let diff = abs (sum - 2 * leftSum)

                let acc =
                    match diff with
                    | 0 -> acc + 2
                    | 1 -> acc + 1
                    | _ -> acc

                countValidSelections' t leftSum sum acc

    countValidSelections' nums 0 (List.sum nums) 0

// 2
countValidSelections [ 1; 0; 2; 0; 3 ]

// 0
countValidSelections [ 2; 3; 4; 0; 4; 1; 0 ]

// 3
countValidSelections [ 16; 13; 10; 0; 0; 0; 10; 6; 7; 8; 7 ]
