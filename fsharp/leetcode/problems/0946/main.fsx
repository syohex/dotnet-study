let validateStackSequences (pushed: int list) (popped: int list) : bool =
    let rec validateStackSequences' pushed popped stack =
        match pushed, popped with
        | [], [] -> true
        | _ :: _, [] -> failwith "never reach here"
        | [], h2 :: t2 ->
            match stack with
            | [] -> failwith "never reach here"
            | top :: rest ->
                if h2 = top then
                    validateStackSequences' pushed t2 rest
                else
                    false
        | h1 :: t1, h2 :: t2 ->
            match stack with
            | top :: rest when top = h2 -> validateStackSequences' pushed t2 rest
            | _ -> validateStackSequences' t1 popped (h1 :: stack)

    validateStackSequences' pushed popped []

// true
validateStackSequences [ 1; 2; 3; 4; 5 ] [
    4
    5
    3
    2
    1
]

// false
validateStackSequences [ 1; 2; 3; 4; 5 ] [
    4
    3
    5
    1
    2
]
