let decimalRepresentation (n: int) : int list =
    let rec decimalRepresentation' n base' acc =
        if n = 0 then
            acc
        else
            let m = n % 10

            if m = 0 then
                decimalRepresentation' (n / 10) (base' * 10) acc
            else
                decimalRepresentation' (n / 10) (base' * 10) ((m * base') :: acc)

    decimalRepresentation' n 1 []

// [500, 30, 7]
decimalRepresentation 537

// [100, 2]
decimalRepresentation 102

// [6]
decimalRepresentation 6
