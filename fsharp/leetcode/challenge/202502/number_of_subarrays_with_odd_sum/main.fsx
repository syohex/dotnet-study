let numOfSubArrays (arr: int list) : int =
    let modulo = 1_000_000_007

    let rec numOfSubArrays' arr evens odds sum acc =
        match arr with
        | [] -> acc
        | h :: t ->
            let sum = sum + h

            if sum % 2 = 0 then
                numOfSubArrays' t (evens + 1) odds sum ((acc + odds) % modulo)
            else
                numOfSubArrays' t evens (odds + 1) sum ((acc + evens) % modulo)

    numOfSubArrays' arr 1 0 0 0

// 4
numOfSubArrays [ 1; 3; 5 ]

// 0
numOfSubArrays [ 2; 4; 6 ]

// 16
numOfSubArrays [ 1; 2; 3; 4; 5; 6; 7 ]
