let rec sortArray (nums: int list) : int list =
    let rec merge left right acc =
        match left, right with
        | [], [] -> List.rev acc
        | h :: t, [] -> merge t [] (h :: acc)
        | [], h :: t -> merge [] t (h :: acc)
        | h1 :: t1, h2 :: t2 ->
            if h1 <= h2 then
                merge t1 right (h1 :: acc)
            else
                merge left t2 (h2 :: acc)

    match nums with
    | [] -> []
    | _ :: [] -> nums
    | _ ->
        let mid = (List.length nums) / 2
        let left = sortArray (List.take mid nums)
        let right = sortArray (List.skip mid nums)
        merge left right []

// [1,2,3,5]
sortArray [ 5; 2; 3; 1 ]

// [0,0,1,1,2,5]
sortArray [ 5; 1; 1; 2; 0; 0 ]
