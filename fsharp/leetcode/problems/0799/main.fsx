let champagneTower (poured: int) (queryRow: int) (queryGlass: int) : double =
    let glasses = Array2D.zeroCreate<double> 101 101
    glasses.[0, 0] <- double poured

    for i in 0 .. queryRow do
        for j in 0 .. queryGlass do
            let v = glasses.[i, j] - 1.0

            if v > 0.0 then
                let half = v / 2.0
                glasses.[i + 1, j] <- glasses.[i + 1, j] + half
                glasses.[i + 1, j + 1] <- glasses.[i + 1, j + 1] + half

    let ret = glasses.[queryRow, queryGlass]
    if ret >= 1.0 then 1.0 else ret

// 0.0
champagneTower 1 1 1

// 0.5
champagneTower 2 1 1

// 1.0
champagneTower 100000009 33 17
