let combinationSum3 (k: int) (n: int) : int list list =
    let rec combinationSum3' num k n acc : int list list =
        if k = 0 then
            if n = 0 then
                [ (acc |> List.rev) ]
            else
                []
        else
            seq { num..9 }
            |> Seq.filter (fun i -> n - i >= 0)
            |> Seq.map (fun i -> combinationSum3' (i + 1) (k - 1) (n - i) (i :: acc))
            |> Seq.fold (fun acc n -> n @ acc) []

    combinationSum3' 1 k n [] |> List.rev

// [[1,2,4]]
combinationSum3 3 7

// [[1,2,6],[1,3,5],[2,3,4]]
combinationSum3 3 9

// []
combinationSum3 4 1

// []
combinationSum3 2 18
