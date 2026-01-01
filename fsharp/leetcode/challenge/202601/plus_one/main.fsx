let plusOne (digits: int list) : int list =
    let rec plusOne' digits carry acc =
        match digits with
        | [] -> if carry = 1 then 1 :: acc else acc
        | h :: t ->
            if h + carry = 10 then
                plusOne' t 1 (0 :: acc)
            else
                plusOne' t 0 ((h + carry) :: acc)

    plusOne' (List.rev digits) 1 []

// [1,2,4]
plusOne [ 1; 2; 3 ]

// [4,3,2,2]
plusOne [ 4; 3; 2; 1 ]

// [1,0]
plusOne [ 9 ]
