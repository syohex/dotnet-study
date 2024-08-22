let findComplement (num: int) : int =
    let rec findComplement' num v acc =
        if num = 0 then
            acc
        else
            let bit = if num % 2 = 0 then 1 else 0
            findComplement' (num / 2) (v * 2) (v * bit + acc)

    findComplement' num 1 0

// 2
findComplement 5

// 0
findComplement 1

// 0
findComplement 15

// 15
findComplement 16
