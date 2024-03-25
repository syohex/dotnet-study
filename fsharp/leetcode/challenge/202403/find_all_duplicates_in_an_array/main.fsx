let findDuplicates (nums: int[]) : int list =
    let rec findDuplicates' i (nums: int[]) acc =
        if i >= nums.Length then
            List.rev acc
        else
            let idx = System.Math.Abs(nums.[i]) - 1

            if nums.[idx] >= 0 then
                let acc' = (idx + 1) :: acc
                nums.[idx] <- nums.[idx] * -1
                findDuplicates' (i + 1) nums acc'
            else
                findDuplicates' (i + 1) nums acc

    Array.iter
        (fun (num: int) ->
            let idx = System.Math.Abs(num) - 1
            nums.[idx] <- nums.[idx] * -1)
        nums

    findDuplicates' 0 nums []

// [3,2]
findDuplicates [| 4; 3; 2; 7; 8; 2; 3; 1 |]

// [1]
findDuplicates [| 1; 1; 2 |]

// []
findDuplicates [||]
