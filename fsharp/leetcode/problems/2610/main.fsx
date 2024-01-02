let findMatrix (nums: int list) : int list list =
    let len = List.length nums

    let rec findMatrix' nums visited acc =
        if Set.count visited >= len then
            acc
        else
            let s, visited' =
                nums
                |> List.fold
                    (fun (s, visited) (i, n) ->
                        if Set.contains i visited || Set.contains n s then
                            s, visited
                        else
                            Set.add n s, Set.add i visited)
                    (Set.empty, visited)

            findMatrix' nums visited' ((Set.toList s) :: acc)

    findMatrix' (List.indexed nums) Set.empty []

findMatrix [ 1; 3; 4; 1; 2; 3; 1 ]

findMatrix [ 1; 2; 3; 4 ]
