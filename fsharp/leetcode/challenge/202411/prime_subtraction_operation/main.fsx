open System

let isPrime (n: int) : bool =
    if n <= 1 then
        false
    else
        let limit = int <| Math.Sqrt(double n)
        seq { 2..limit } |> Seq.forall (fun m -> n % m <> 0)

let primeSubOperation (nums: int list) : bool =
    let rec primeSubOperation' nums prev (primes: int[]) =
        match nums with
        | [] -> true
        | h :: t ->
            let diff = h - prev

            if diff <= 0 then
                false
            else
                let prev = h - primes.[diff - 1]
                primeSubOperation' t prev primes

    let maxVal = List.max nums

    let nearestPrimes =
        seq { 0..maxVal }
        |> Seq.fold (fun (acc, prev) n -> if isPrime n then n :: acc, n else prev :: acc, prev) ([], 0)
        |> fst
        |> List.rev
        |> List.toArray

    primeSubOperation' nums 0 nearestPrimes

// true
primeSubOperation [ 4; 9; 6; 10 ]

// true
primeSubOperation [ 6; 8; 11; 12 ]

// false
primeSubOperation [ 5; 8; 3 ]
