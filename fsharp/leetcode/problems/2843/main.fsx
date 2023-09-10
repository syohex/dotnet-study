let countSymmetricIntegers (low: int) (high: int) : int =
    let rec countSymmetricIntegers' i high acc =
        if i > high then
            acc
        else if i >= 0 && i <= 9 then
            countSymmetricIntegers' 10 high acc
        elif i >= 100 && i <= 999 then
            countSymmetricIntegers' 1000 high acc
        elif i >= 10 && i <= 99 then
            let acc' = if i / 10 = i % 10 then acc + 1 else acc
            countSymmetricIntegers' (i + 1) high acc'
        else
            let a = i / 1000 + (i % 1000) / 100
            let b = (i % 100) / 10 + i % 10
            let acc' = if a = b then acc + 1 else acc
            countSymmetricIntegers' (i + 1) high acc'

    countSymmetricIntegers' low high 0

// 9
countSymmetricIntegers 1 100

// 4
countSymmetricIntegers 1200 1230
