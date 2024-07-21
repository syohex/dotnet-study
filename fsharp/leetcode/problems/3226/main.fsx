let minChanges (n: int) (k: int) : int =
    let rec minChanges' n k mask acc i =
        if i >= 32 then
            acc
        else
            let a = n &&& mask
            let b = k &&& mask

            if a <> 0 && b = 0 then
                minChanges' n k (mask <<< 1) (acc + 1) (i + 1)
            elif a = 0 && b <> 0 then
                -1
            else
                minChanges' n k (mask <<< 1) acc (i + 1)

    minChanges' n k 1 0 0

// 2
minChanges 13 4

// 0
minChanges 21 21

// -1
minChanges 14 13
