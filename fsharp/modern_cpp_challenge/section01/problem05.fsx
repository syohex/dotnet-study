let isPrime (n: int) : bool =
    let rec isPrime' n m limit =
        if m > limit then true
        else if n % m = 0 then false
        elif m = 2 then isPrime' n (m + 1) limit
        else isPrime' n (m + 2) limit

    if n = 1 then
        false
    else
        let limit = (float >> sqrt >> int) n
        isPrime' n 2 limit

let sexyPrimes (n: int) : (int * int) list =
    let primes = [ 1 .. n ] |> List.filter isPrime
    let s = primes |> Set.ofList

    primes
    |> List.filter (fun n -> Set.contains (n + 6) s)
    |> List.map (fun n -> n, n + 6)

sexyPrimes 50
sexyPrimes 100