let digitSum (n: int) : int =
    let rec digitSum' n sum =
        if n = 0 then
            sum
        else
            let m = n % 10
            digitSum' (n / 10) (sum + (m * m))

    digitSum' n 0

let isHappy (n: int) : bool =
    let rec isHappy' n s =
        let sum = digitSum n

        if Set.contains sum s then false
        else if sum = 1 then true
        else isHappy' sum (Set.add sum s)

    isHappy' n Set.empty

digitSum 19
digitSum 82
digitSum 100

isHappy 19
isHappy 2
