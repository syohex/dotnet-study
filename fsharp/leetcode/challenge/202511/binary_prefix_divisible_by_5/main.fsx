let prefixesDivBy5 (nums: int list) : bool list =
    let rec prefixesDivBy5' nums sum acc =
        match nums with
        | [] -> List.rev acc
        | h :: t ->
            let sum = (sum * 2 + h) % 5
            prefixesDivBy5' t sum ((sum = 0) :: acc)

    prefixesDivBy5' nums 0 []

// [true, false, false]
prefixesDivBy5 [ 0; 1; 1 ]

// [false, false, false]
prefixesDivBy5 [ 1; 1; 1 ]
