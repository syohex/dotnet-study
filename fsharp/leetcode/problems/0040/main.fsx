let combinationSum2 (candidates: int list) (target: int) : int list list =
    let rec combinationSum2' candidates sum acc ret =
        if sum = target then
            Set.add (acc |> List.sort) ret
        else
            match candidates with
            | [] -> ret
            | h :: t ->
                let ret' =
                    if (sum + h) <= target then
                        combinationSum2' t (sum + h) (h :: acc) ret
                    else
                        ret

                combinationSum2' t sum acc ret'

    combinationSum2' candidates 0 [] Set.empty
    |> Set.toList

// [[1;1;6],[1;2;5],[1;7],[2;6]]
combinationSum2 [ 10; 1; 2; 7; 6; 1; 5 ] 8

// [[1;2;2],[5]]
combinationSum2 [ 2; 5; 2; 1; 2 ] 5
