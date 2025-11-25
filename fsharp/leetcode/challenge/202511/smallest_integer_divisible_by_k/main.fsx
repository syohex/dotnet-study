let smallestRepunitDivByK (k: int) : int =
    let rec smallestRepunitDivByK' n k digits visited =
        let m = (10 * n + 1) % k

        if m = 0 then
            digits
        else if Set.contains m visited then
            -1
        else
            smallestRepunitDivByK' m k (digits + 1) (Set.add m visited)

    smallestRepunitDivByK' 0 k 1 Set.empty

// 1
smallestRepunitDivByK 1

// -1
smallestRepunitDivByK 2

// 3
smallestRepunitDivByK 3
