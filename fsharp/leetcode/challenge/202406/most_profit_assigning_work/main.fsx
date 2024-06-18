let maxProfitAssignment (difficulty: int list) (profit: int list) (worker: int list) : int =
    let rec setMaxValue i prev (v: (int * int)[]) =
        if i >= v.Length then
            v
        else
            v.[i] <- (fst v.[i], max prev (snd v.[i]))
            setMaxValue (i + 1) (snd v.[i]) v

    let rec lowerBound worker profit left right (difficultyProfit: (int * int)[]) =
        if left > right then
            profit
        else
            let mid = left + (right - left) / 2
            let (d, p) = difficultyProfit.[mid]

            if d <= worker then
                lowerBound worker (max profit p) (mid + 1) right difficultyProfit
            else
                lowerBound worker profit left (mid - 1) difficultyProfit

    let rec maxProfitAssignment' worker (difficultyProfit: (int * int)[]) ret =
        match worker with
        | [] -> ret
        | h :: t ->
            let v = lowerBound h 0 0 (difficultyProfit.Length - 1) difficultyProfit
            maxProfitAssignment' t difficultyProfit (ret + v)

    let difficultyProfit =
        (0, 0) :: (List.zip difficulty profit |> List.sort) |> List.toArray

    let difficultyProfit = setMaxValue 1 0 difficultyProfit
    maxProfitAssignment' worker difficultyProfit 0

// 100
maxProfitAssignment [ 2; 4; 6; 8; 10 ] [ 10; 20; 30; 40; 50 ] [ 4; 5; 6; 7 ]

// 0
maxProfitAssignment [ 85; 47; 57 ] [ 24; 66; 99 ] [ 40; 25; 25 ]

// 324
maxProfitAssignment [ 68; 35; 52; 47; 86 ] [ 67; 17; 1; 81; 3 ] [ 92; 10; 85; 84; 82 ]

// 172
maxProfitAssignment [ 49; 49; 76; 88; 100 ] [ 5; 8; 75; 89; 94 ] [ 98; 72; 16; 27; 76 ]

// 765
maxProfitAssignment
    [ 5; 50; 92; 21; 24; 70; 17; 63; 30; 53 ]
    [ 68; 100; 3; 99; 56; 43; 26; 93; 55; 25 ]
    [ 96; 3; 55; 30; 11; 58; 68; 36; 26; 1 ]
