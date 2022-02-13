let rec subsets (nums: int list) : int list list =
    match nums with
    | [] -> [ [] ]
    | head :: tail ->
        let rest = subsets tail
        let prepends = rest |> List.map (fun s -> [ head ] @ s)
        prepends @ rest

// [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]: answer in any order
subsets [ 1; 2; 3 ]

// [[], [0]]
subsets [ 0 ]
