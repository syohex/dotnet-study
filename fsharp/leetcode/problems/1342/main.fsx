let numberOfSteps (num: int) : int =
    let rec numberOfSteps' num ret =
        if num = 0 then
            ret
        elif num % 2 = 0 then
            numberOfSteps' (num / 2) (ret + 1)
        else
            numberOfSteps' (num - 1) (ret + 1)

    numberOfSteps' num 0

// 6
numberOfSteps 14

// 4
numberOfSteps 8

// 12
numberOfSteps 123
