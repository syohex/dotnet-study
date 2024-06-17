let judgeSquareSum (c: int) : bool =
    let rec search (left: int64) (right: int64) (target: int64) : bool =
        if left > right then
            false
        else
            let mid = left + (right - left) / 2L
            let v = mid * mid

            if v = target then true
            else if v < target then search (mid + 1L) right target
            else search left (mid - 1L) target

    let rec judgeSquareSum' (a: int64) (c: int64) =
        if a * a > c then
            false
        else
            let diff = c - (a * a)

            if search a diff diff then
                true
            else
                judgeSquareSum' (a + 1L) c

    judgeSquareSum' 0L (int64 c)

// true
judgeSquareSum 5

// false
judgeSquareSum 3

// true
judgeSquareSum 1000000
