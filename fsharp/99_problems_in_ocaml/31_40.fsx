// problem 31
let isPrime n =
    let divs =
        seq { 1 .. n }
        |> Seq.filter (fun m -> n % m = 0)
        |> Seq.toList

    divs = [ 1; n ]

// true
not (isPrime 1)

// true
isPrime 7

// true
not (isPrime 12)

Seq.initInfinite id |> Seq.filter isPrime |> Seq.take 10 |> Seq.toList
