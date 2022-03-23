let brokenCalc (startValue: int) (target: int) : int =
    let rec brokenCalc' startValue target count =
        if target <= startValue then
            count + startValue - target
        else
            if target % 2 = 1 then
                brokenCalc' startValue (target + 1) (count + 1)
            else
                brokenCalc' startValue (target / 2) (count + 1)

    brokenCalc' startValue target 0

// 2
brokenCalc 2 3

// 2
brokenCalc 5 8

// 3
brokenCalc 3 10
