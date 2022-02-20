let countEven (num: int) : int =
    let digitSum n =
        let rec digitSum' n acc =
            if n = 0 then
                acc
            else
                digitSum' (n / 10) (acc + (n % 10))

        digitSum' n 0

    seq { 1..num }
    |> Seq.filter (fun n -> (digitSum n) % 2 = 0)
    |> Seq.length

// 2
countEven 4

// 14
countEven 30
