let sumFourDivisors (nums: int list) : int =
    let sumDivisors n =
        let rec sumDivisors' nums divisors acc =
            if divisors > 4 then
                -1
            else
                match nums with
                | [] -> if divisors = 4 then acc else -1
                | h :: t ->
                    if n % h = 0 then
                        let divisors = divisors + 1
                        let j = n / h

                        if h <> j then
                            sumDivisors' t (divisors + 1) (acc + h + j)
                        else
                            sumDivisors' t divisors acc
                    else
                        sumDivisors' t divisors acc

        let limit = n |> double |> sqrt |> int
        let nums = seq { 2..limit } |> Seq.toList
        sumDivisors' nums 2 (1 + n)

    nums
    |> List.fold
        (fun acc n ->
            let d = sumDivisors n
            if d <> -1 then acc + d else acc)
        0

// 32
sumFourDivisors [ 21; 4; 7 ]

// 64
sumFourDivisors [ 21; 21 ]

// 0
sumFourDivisors [ 1; 2; 3; 4; 5 ]
