let lexicalOrder (n: int) : int list =
    let rec nextNumber v n =
        if v % 10 = 9 || v >= n then
            nextNumber (v / 10) n
        else
            v + 1

    let rec lexicalOrder' i v n acc =
        if i > n then
            List.rev acc
        else
            let acc = v :: acc

            if v * 10 <= n then
                lexicalOrder' (i + 1) (v * 10) n acc
            else
                lexicalOrder' (i + 1) (nextNumber v n) n acc

    lexicalOrder' 1 1 n []

// [1,10,11,12,13,2,3,4,5,6,7,8,9]
lexicalOrder 13

// [1, 2]
lexicalOrder 2
