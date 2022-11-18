let isUgly (n: int) : bool =
    let rec div divisor (n: int) =
        if n % divisor <> 0 then
            n
        else
            div divisor (n / divisor)

    if n <= 0 then
        false
    else
        (n |> div 2 |> div 3 |> div 5) = 1

// true
isUgly 6

// true
isUgly 1

// false
isUgly 0

// false
isUgly -10
