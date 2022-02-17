let combinationSum (nums: int list) (target: int) : int list list =
    let rec combinationSum' i (nums: int list) target ys ret =
        let sum = ys |> List.sum

        if sum = target then
            (List.rev ys) :: ret
        else if sum > target || i >= (List.length nums) then
            ret
        else
            nums
            |> List.mapi (fun i num -> (i, num))
            |> List.skip i
            |> List.fold (fun acc (i, num) -> combinationSum' i nums target (num :: ys) acc) ret

    combinationSum' 0 (nums |> List.sort) target [] []
    |> List.rev

// [[2,2,3],[7]]
combinationSum [ 2; 3; 6; 7 ] 7

// [[2,2,2,2],[2,3,3],[3,5]]
combinationSum [ 2; 3; 5 ] 8

// []
combinationSum [ 2 ] 1
