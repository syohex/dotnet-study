let distributeCandies (n: int) (limit: int) : int =
    let rec distributeCandies' n limit people =
        if people >= 3 then
            if n = 0 then 1 else 0
        else
            let limit' = System.Math.Min(n, limit)

            seq { 0..limit' }
            |> Seq.fold (fun acc i -> acc + distributeCandies' (n - i) limit (people + 1)) 0

    distributeCandies' n limit 0

// 3
distributeCandies 5 2

// 10
distributeCandies 3 3
