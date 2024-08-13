let combinationSum2 (candidates: int list) (target: int) : int list list =
    let rec combinationSum2' pos sum (candidates: int[]) acc ret =
        if sum = target then
            (List.rev acc) :: ret
        else
            seq { pos .. (candidates.Length - 1) }
            |> Seq.fold
                (fun ret i ->
                    if sum + candidates.[i] > target then
                        ret
                    elif pos = i || candidates.[i] <> candidates.[i - 1] then
                        combinationSum2' (i + 1) (sum + candidates.[i]) candidates (candidates.[i] :: acc) ret
                    else
                        ret)
                ret

    let candidates = candidates |> List.sort |> List.toArray
    combinationSum2' 0 0 candidates [] [] |> List.rev

// [[1,1,6],[1,2,5],[1,7],[2,6]]
combinationSum2 [ 10; 1; 2; 7; 6; 1; 5 ] 8

// [[1;2;2],[5]]
combinationSum2 [ 2; 5; 2; 1; 2 ] 5
