let minimumReplacement (nums: int list) : int64 =
    let rec minimumReplacement' nums next ret =
        match nums with
        | [] -> ret
        | h :: t ->
            if h <= next then
                minimumReplacement' t h ret
            else
                let divides = if h % next = 0 then h / next - 1 else h / next
                let next' = h / (divides + 1)
                minimumReplacement' t next' (ret + int64 divides)

    let nums' = List.rev nums
    minimumReplacement' (List.tail nums') (List.head nums') 0

// 2
minimumReplacement [ 3; 9; 3 ]

// 0
minimumReplacement [ 1; 2; 3; 4; 5 ]

// 6
minimumReplacement [ 12; 9; 7; 6; 17; 19; 21 ]
