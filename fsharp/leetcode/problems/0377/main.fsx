let combinationSum4 (nums: int list) (target: int) : int =
    let rec combinationSum4' i nums target (dp: int []) =
        if i > target then
            dp.[target]
        else
            nums
            |> List.iter (fun num ->
                if i - num >= 0 then
                    dp.[i] <- dp.[i] + dp.[i - num])

            combinationSum4' (i + 1) nums target dp

    let dp = Array.zeroCreate (target + 1)
    dp.[0] <- 1
    combinationSum4' 1 nums target dp

// 7
combinationSum4 [ 1; 2; 3 ] 4

// 0
combinationSum4 [ 9 ] 3
