let validateStackSequences (pushed: int list) (popped: int list) : bool =
    let rec validateStackSequences' pushed popped stack =
        match pushed, popped with
        | [], [] -> true
        | _, [] -> failwith "never reach here"
        | [], h :: t ->
            match stack with
            | [] -> failwith "never reach here"
            | h2 :: t2 ->
                if h = h2 then
                    validateStackSequences' pushed t t2
                else
                    false
        | h1 :: t1, h2 :: t2 ->
            match stack with
            | [] -> validateStackSequences' t1 popped (h1 :: stack)
            | h3 :: t3 ->
                if h2 = h3 then
                    validateStackSequences' pushed t2 t3
                else
                    validateStackSequences' t1 popped (h1 :: stack)

    validateStackSequences' pushed popped []

// true
validateStackSequences [ 1; 2; 3; 4; 5 ] [ 4; 5; 3; 2; 1 ]

// false
validateStackSequences [ 1; 2; 3; 4; 5 ] [ 4; 3; 5; 1; 2 ]
