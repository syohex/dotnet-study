let subsets (nums: int list) : int list list =
    let rec subsets' nums acc =
        match nums with
        | [] -> acc |> List.rev
        | h :: t ->
            let acc' = acc |> List.fold (fun acc' v -> (v @ [ h ]) :: acc') acc
            subsets' t acc'

    subsets' nums [ [] ]

// [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]
subsets [ 1; 2; 3 ]

// [[],[0]]
subsets [ 0 ]
