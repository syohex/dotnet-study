let pivotInteger (n: int) : int =
    let rec pivotInteger' i n sum total =
        if i > n then
            -1
        else
            let sum' = sum + i

            if sum' = total - sum then
                i
            else
                pivotInteger' (i + 1) n sum' total

    let total = (1 + n) * n / 2
    pivotInteger' 1 n 0 total

// 6
pivotInteger 8

// 1
pivotInteger 1

// -1
pivotInteger 4
