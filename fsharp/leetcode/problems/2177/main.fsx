let sumOfThree (num: int64) : int64 list =
    if num % 3L = 0 then
        let n = num / 3L
        [ n - 1L; n; n + 1L ]
    else
        []

// [10; 11; 12]
sumOfThree 33

// []
sumOfThree 4
