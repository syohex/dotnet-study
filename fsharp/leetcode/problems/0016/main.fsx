let threeSumClosest (nums: int list) (target: int) : int =
    let rec findMinDiff (nums: int []) i left right target ret =
        if left >= right then
            ret
        else
            let sum = nums.[i] + nums.[left] + nums.[right]

            if sum = target then
                0
            else
                let ret' =
                    if System.Math.Abs(target - sum) < System.Math.Abs(ret) then
                        target - sum
                    else
                        ret

                if sum < target then
                    findMinDiff nums i (left + 1) right target ret'
                else
                    findMinDiff nums i left (right - 1) target ret'


    let rec threeSumClosest' i (nums: int []) target minDiff =
        if i >= nums.Length then
            target - minDiff
        else
            let minDiff' =
                findMinDiff nums i (i + 1) (nums.Length - 1) target System.Int32.MaxValue

            if minDiff' = 0 then
                target
            else
                let minDiff'' =
                    if System.Math.Abs(minDiff') < System.Math.Abs(minDiff) then
                        minDiff'
                    else
                        minDiff

                threeSumClosest' (i + 1) nums target minDiff''

    let nums' = nums |> List.sort |> List.toArray
    threeSumClosest' 0 nums' target System.Int32.MaxValue


// 2
threeSumClosest [ -1; 2; 1; -4 ] 1

// 0
threeSumClosest [ 0; 0; 0 ] 1
