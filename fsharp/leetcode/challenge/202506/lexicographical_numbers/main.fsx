let lexicalOrder (n: int) : int list =
    let rec divNum m =
        if m % 10 = 9 || m >= n then divNum (m / 10) else m

    let rec lexicalOrder' i v acc =
        if i = n then
            List.rev acc
        else
            let acc = v :: acc

            if v * 10 <= n then
                lexicalOrder' (i + 1) (v * 10) acc
            else
                let v = divNum v
                lexicalOrder' (i + 1) (v + 1) acc

    lexicalOrder' 0 1 []

lexicalOrder 13

lexicalOrder 2
