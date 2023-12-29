let numberGame (nums: int list) : int list =
    let rec numberGame' nums acc =
        match nums with
        | [] -> List.rev acc
        | _ :: [] -> failwith "never reach here"
        | a :: b :: t -> numberGame' t (a :: b :: acc)

    numberGame' (List.sort nums) []

// [3,2,5,4]
numberGame [ 5; 4; 2; 3 ]

// [5,2]
numberGame [ 2; 5 ]
