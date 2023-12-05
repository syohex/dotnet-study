let numberOfMatches (n: int) : int =
    let rec numberOfMatches' n isEven acc =
        if n <= 1 then
            acc
        else
            let matches = if isEven then n / 2 else (n - 1) / 2
            numberOfMatches' (n - matches) (not isEven) (acc + matches)

    numberOfMatches' n true 0

// 6
numberOfMatches 7

// 13
numberOfMatches 14
