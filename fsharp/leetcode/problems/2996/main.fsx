let missingIntger (nums: int list) : int =
    let rec findPrefixSum nums prev sum =
        match nums with
        | [] -> sum
        | h :: t -> if h = prev + 1 then findPrefixSum t h (sum + h) else sum

    let rec missingIntger' num s =
        if Set.contains num s then
            missingIntger' (num + 1) s
        else
            num

    match nums with
    | [] -> failwith "never reach here"
    | h :: t ->
        let sum = findPrefixSum t h h
        let s = Set.ofList nums

        missingIntger' sum s

// 6
missingIntger [ 1; 2; 3; 2; 5 ]

// 15
missingIntger [ 3; 4; 5; 1; 12; 14; 13 ]

// 297
missingIntger [ 29..37 ]

// 30
missingIntger [ 4; 5; 6; 7; 8; 8; 9; 4; 3; 2; 7 ]

// 38
missingIntger [ 37; 1; 2; 9; 5; 8; 5; 2; 9; 4 ]

// 15
missingIntger [ 14; 9; 6; 7; 9; 10; 4; 9; 9; 4; 4 ]
