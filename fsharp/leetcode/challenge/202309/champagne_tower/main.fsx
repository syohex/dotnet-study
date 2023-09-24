let champagneTower (poured: int) (queryRow: int) (queryGlass: int) : double =
    let glasses: double[,] = Array2D.zeroCreate (queryRow + 2) (queryGlass + 2)
    glasses.[0, 0] <- poured

    for i in 0..queryRow do
        for j in 0..queryGlass do
            let half = (glasses.[i, j] - 1.0) / 2.0

            if half > 0 then
                glasses.[i + 1, j] <- glasses.[i + 1, j] + half
                glasses.[i + 1, j + 1] <- glasses.[i + 1, j + 1] + half

    System.Math.Min(1.0, glasses.[queryRow, queryGlass])

// 0
champagneTower 1 1 1

// 0.5
champagneTower 2 1 1

// 1
champagneTower 100000009 33 17
