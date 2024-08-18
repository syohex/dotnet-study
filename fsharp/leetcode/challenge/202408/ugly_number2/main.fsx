let nthUglyNumber (n: int) : int =
    let rec nthUglyNumber' i n (v: int[]) i2 i3 i5 =
        if i >= n then
            v.[n - 1]
        else
            let v2, v3, v5 = 2 * v.[i2], 3 * v.[i3], 5 * v.[i5]
            let minV = min v2 (min v3 v5)
            let i2 = if minV = v2 then i2 + 1 else i2
            let i3 = if minV = v3 then i3 + 1 else i3
            let i5 = if minV = v5 then i5 + 1 else i5
            v.[i] <- minV
            nthUglyNumber' (i + 1) n v i2 i3 i5

    let v = Array.zeroCreate n
    v.[0] <- 1
    nthUglyNumber' 1 n v 0 0 0

// 12
nthUglyNumber 10

// 1
nthUglyNumber 1
