let arraySign (nums: int list) : int =
    let rec arraySign' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            if h > 0 then arraySign' t acc
            elif h = 0 then 0
            else arraySign' t (-1 * acc)

    arraySign' nums 1

// 1
arraySign [ -1; -2; -3; -4; 3; 2; 1 ]

// 0
arraySign [ 1; 5; 0; 2; -3 ]

// -1
arraySign [ -1; 1; -1; 1; -1 ]
