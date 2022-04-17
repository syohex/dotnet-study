let findCloseNumber (nums: int list) : int =
    nums
    |> List.fold
        (fun min num ->
            let absMin = System.Math.Abs(min)
            let absNum = System.Math.Abs(num)

            if absNum < absMin || (absNum = absMin && num > min) then
                num
            else
                min)
        System.Int32.MaxValue

// 1
findCloseNumber [ -4; -2; 1; 4; 8 ]

// 1
findCloseNumber [ 2; -1; 1 ]

// -1
findCloseNumber [ 2; -1; -2 ]

// 0
findCloseNumber [ 0 ]

// 50
findCloseNumber [ -50; 50 ]
