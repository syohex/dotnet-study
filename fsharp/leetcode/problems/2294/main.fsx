let partionArray (nums: int list) (k: int) : int =
    let rec partionArray' nums k max ret =
        match nums with
        | [] -> ret
        | h :: t ->
            if max - h > k then
                partionArray' t k h (ret + 1)
            else
                partionArray' t k max ret

    let nums' = nums |> List.sort |> List.rev
    partionArray' nums'.Tail k nums'.Head 1

// 2
partionArray [ 3; 6; 1; 2; 5 ] 2

// 2
partionArray [ 1; 2; 3 ] 1

// 3
partionArray [ 2; 2; 4; 5 ] 0
