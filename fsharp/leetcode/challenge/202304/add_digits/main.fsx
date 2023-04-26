let addDigits (num: int) : int =
    let rec digitSum n acc =
        if n = 0 then acc else digitSum (n / 10) (acc + (n % 10))

    let rec addDigits' num =
        if num < 10 then num else addDigits' (digitSum num 0)

    addDigits' num

// 2
addDigits 38

// 0
addDigits 0
