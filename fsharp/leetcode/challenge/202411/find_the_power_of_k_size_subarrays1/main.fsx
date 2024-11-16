let resultArrays (nums: int list) (k: int) : int list =
    let rec countConsectives nums prev acc =
        match nums with
        | [] -> acc
        | h :: t -> if h = prev - 1 then countConsectives t h (acc + 1) else acc

    let rec resultArrays nums prev consectives acc =
        match nums with
        | [] -> List.rev acc
        | h :: t ->
            let consectives = if consectives = k then consectives - 1 else consectives
            let consectives = if h = prev + 1 then consectives + 1 else 1

            if consectives = k then
                resultArrays t h consectives (h :: acc)
            else
                resultArrays t h consectives (-1 :: acc)

    let tmp = List.take k nums |> List.rev
    let prev = List.head tmp
    let consectives = countConsectives (List.tail tmp) prev 1
    let acc = if consectives = k then [ prev ] else [ -1 ]
    resultArrays (List.skip k nums) prev consectives acc

// [3,4,-1,-1,-1]
resultArrays [ 1; 2; 3; 4; 3; 2; 5 ] 3

// [-1,-1]
resultArrays [ 2; 2; 2; 2; 2 ] 4

// [-1,3,-1,-3,-1]
resultArrays [ 3; 2; 3; 2; 3; 2 ] 2

// [-1,4]
resultArrays [ 1; 3; 4 ] 2
