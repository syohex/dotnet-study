let fillCups ((a, b, c): (int * int * int)) : int =
    let sort a b c =
        let sorted = [ a; b; c ] |> List.sort |> List.rev
        List.item 0 sorted, List.item 1 sorted, List.item 2 sorted

    let rec fillCups' (a, b, c) ret =
        if a = 0 then
            ret
        else
            fillCups' (sort (a - 1) (b - 1) c) (ret + 1)

    fillCups' (a, b, c) 0

// 4
fillCups (1, 4, 2)

// 7
fillCups (5, 4, 4)

// 5
fillCups (5, 0, 0)
