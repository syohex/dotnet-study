let primes (left: int) (right: int) : bool[] =
    let rec fillPrimes n i limit (ps: bool[]) =
        let index = n * i

        if index > limit then
            ps
        else
            ps.[index] <- false
            fillPrimes n (i + 1) limit ps

    let rec primes' n (ps: bool[]) =
        if n > right then
            ps
        else if not ps.[n] then
            primes' (n + 1) ps
        else
            let ps = fillPrimes n 2 right ps
            primes' (n + 1) ps

    let ps = Array.init (right + 1) (fun _ -> true)
    ps.[0] <- false
    ps.[1] <- false
    primes' 2 ps

let closestPrimes (left: int) (right: int) : int * int =
    let rec closestPrimes' i (primes: bool[]) minDiff (prev2, prev1) acc =
        if i > right then
            acc
        else if primes.[i] then
            let prev2 = prev1
            let prev1 = i

            if prev2 <> -1 && prev1 <> -1 && prev1 - prev2 < minDiff then
                closestPrimes' (i + 1) primes (prev1 - prev2) (prev2, prev1) (prev2, prev1)
            else
                closestPrimes' (i + 1) primes minDiff (prev2, prev1) acc
        else
            closestPrimes' (i + 1) primes minDiff (prev2, prev1) acc

    let ps = primes left right
    closestPrimes' left ps System.Int32.MaxValue (-1, -1) (-1, -1)

// (11, 13)
closestPrimes 10 19

// (-1, -1)
closestPrimes 4 6
