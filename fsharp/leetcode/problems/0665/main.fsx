let checkPossibility (nums: int list) : bool =
    let rec checkPossibility' nums prev2 prev1 count =
        if count >= 2 then
            false
        else
            match nums with
            | [] -> true
            | h :: t ->
                if h >= prev1 then
                    checkPossibility' t prev1 h count
                else if prev2 > h then
                    checkPossibility' t prev1 prev1 (count + 1)
                else
                    checkPossibility' t h h (count + 1)

    match nums with
    | []
    | _ :: []
    | _ :: _ :: [] -> true
    | h1 :: h2 :: rest ->
        if h1 > h2 then
            checkPossibility' rest h2 h2 1
        else
            checkPossibility' rest h1 h2 0

// true
checkPossibility [ 4; 2; 3 ]

// false
checkPossibility [ 4; 2; 1 ]

// false
checkPossibility [ 3; 4; 2; 3 ]

// true
checkPossibility [ 5; 7; 1; 8 ]

// true
checkPossibility [ -1; 4; 2; 3 ]

// false
checkPossibility [ 1; 2; 5; 4; 3 ]

// true
checkPossibility [ 1; 2; 7; 2; 5; 9 ]
