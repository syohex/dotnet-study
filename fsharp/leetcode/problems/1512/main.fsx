let numIdenticalPairs (nums: int list) : int =
    let rec numIdenticalPairs' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let matched = List.filter ((=) h) t |> List.length
            numIdenticalPairs' t (matched + acc)

    numIdenticalPairs' nums 0

// 4
numIdenticalPairs [ 1; 2; 3; 1; 1; 3 ]

// 6
numIdenticalPairs [ 1; 1; 1; 1 ]

// 0
numIdenticalPairs [ 1; 2; 3 ]
