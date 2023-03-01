let sortArray (nums: int list) : int list =
    let rec merge left right acc =
        match left, right with
        | [], [] -> List.rev acc
        | h :: t, [] -> merge t [] (h :: acc)
        | [], h :: t -> merge [] t (h :: acc)
        | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then
                merge t1 right (h1 :: acc)
            else
                merge left t2 (h2 :: acc)

    let rec sortArray' nums len =
        if len = 1 then
            nums
        else
            let len' = len / 2
            let front, back = List.splitAt len' nums
            let left = sortArray' front len'
            let right = sortArray' back len'

            merge left right []

    sortArray' nums (List.length nums)

// [1;2;3;5]
sortArray [ 5; 2; 3; 1 ]

// [5;1;1;2;0;0]
sortArray [ 0; 0; 1; 1; 2; 5 ]

// [3;2;1]
sortArray [ 1; 2; 3 ]
