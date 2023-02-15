let addToArrayForm (num: int list) (k: int) : int list =
    let rec addToArrayForm' nums ks carry acc =
        match nums, ks with
        | [], [] -> if carry = 1 then 1 :: acc else acc
        | h :: t, [] ->
            let v = h + carry

            if v >= 10 then
                addToArrayForm' t [] 1 ((v - 10) :: acc)
            else
                addToArrayForm' t [] 0 (v :: acc)
        | [], h :: t ->
            let v = h + carry

            if v >= 10 then
                addToArrayForm' [] t 1 ((v - 10) :: acc)
            else
                addToArrayForm' [] t 0 (v :: acc)
        | h1 :: t1, h2 :: t2 ->
            let v = h1 + h2 + carry

            if v >= 10 then
                addToArrayForm' t1 t2 1 ((v - 10) :: acc)
            else
                addToArrayForm' t1 t2 0 (v :: acc)

    let ks = k |> string |> Seq.map (fun c -> int c - int '0') |> Seq.rev |> Seq.toList
    addToArrayForm' (num |> List.rev) ks 0 []

// [1;2;3;4]
addToArrayForm [ 1; 2; 0; 0 ] 34

// [4;5;5]
addToArrayForm [ 2; 7; 4 ] 181

// [1;0;2;1]
addToArrayForm [ 2; 1; 5 ] 806
