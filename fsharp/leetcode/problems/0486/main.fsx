open System

let predictTheWinner (nums: int list) : bool =
    let rec predictTheWinner' (nums: int[]) left right =
        if left >= right then
            nums.[left]
        else
            let leftMax = nums.[left] - (predictTheWinner' nums (left + 1) right)
            let rightMax = nums.[right] - (predictTheWinner' nums left (right - 1))
            Math.Max(leftMax, rightMax)

    let nums' = Array.ofList nums
    let maxScore = predictTheWinner' nums' 0 (nums'.Length - 1)
    maxScore >= 0

// false
predictTheWinner [ 1; 5; 2 ]

// true
predictTheWinner [ 1; 5; 233; 7 ]
