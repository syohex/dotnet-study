let numberOfArithmeticSlices (nums: int list) : int =
    let countSlices count =
        if count < 3 then
            0
        else
            let n = count - 3 + 1
            n * (1 + n) / 2

    let rec numberOfArithmeticSlices' nums prev2 prev1 count ret =
        match nums with
        | [] -> ret + countSlices count
        | x :: xs ->
            if (x - prev1) = (prev1 - prev2) then
                numberOfArithmeticSlices' xs prev1 x (count + 1) ret
            else
                numberOfArithmeticSlices' xs prev1 x 2 (ret + countSlices count)

    match nums with
    | []
    | _ :: []
    | _ :: _ :: [] -> 0
    | prev2 :: prev1 :: xs -> numberOfArithmeticSlices' xs prev2 prev1 2 0

// 3
numberOfArithmeticSlices [ 1; 2; 3; 4 ]

// 3
numberOfArithmeticSlices [ 7; 7; 7; 7 ]

// 6
numberOfArithmeticSlices [ 1
                           3
                           5
                           7
                           9 ]

// 0
numberOfArithmeticSlices [ 1 ]
