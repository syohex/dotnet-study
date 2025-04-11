let countSummetricIntegers (low: int) (high: int) : int =
    seq { low..high }
    |> Seq.filter (fun n ->
        if n >= 1000 then
            let sum1 = n / 1000 + (n % 1000) / 100
            let sum2 = (n % 100) / 10 + n % 10
            sum1 = sum2
        elif n >= 100 then
            false
        else
            n % 11 = 0)
    |> Seq.length

// 9
countSummetricIntegers 1 100

// 4
countSummetricIntegers 1200 1230
