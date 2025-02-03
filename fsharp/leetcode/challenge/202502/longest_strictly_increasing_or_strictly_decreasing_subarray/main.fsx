let longestMonotonicSubarray (nums: int list) : int =
    let rec f nums prev count isIncreasing acc =
        match nums with
        | [] -> acc
        | h :: t ->
            if count = 1 then
                if h > prev then f t h 2 true (max acc 2)
                elif h < prev then f t h 2 false (max acc 2)
                else f t h 1 isIncreasing acc
            else if h = prev then
                f t h 1 isIncreasing acc
            elif h > prev then
                if isIncreasing then
                    f t h (count + 1) true (max acc (count + 1))
                else
                    f t h 2 true (max acc 2)
            else if isIncreasing then
                f t h 2 false (max acc 2)
            else
                f t h (count + 1) false (max acc (count + 1))

    match nums with
    | [] -> failwith "never reach here"
    | h :: t -> f t h 1 true 1

// 2
longestMonotonicSubarray [ 1; 4; 3; 3; 2 ]

// 1
longestMonotonicSubarray [ 3; 3; 3; 3 ]

// 3
longestMonotonicSubarray [ 3; 2; 1 ]

// 3
longestMonotonicSubarray [ 1; 9; 7; 1 ]
