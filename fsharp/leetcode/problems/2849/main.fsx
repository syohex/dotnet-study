open System

let isReachableAtTime (sx: int) (sy: int) (fx: int) (fy: int) (t: int) : bool =
    let xDist = Math.Abs(sx - fx)
    let yDist = Math.Abs(sy - fy)

    if xDist = 0 && yDist = 0 && t = 1 then
        false
    else
        t >= Math.Max(xDist, yDist)

// true
isReachableAtTime 2 4 7 7 6

// false
isReachableAtTime 3 1 7 3 3

// false
isReachableAtTime 1 1 1 1 1
