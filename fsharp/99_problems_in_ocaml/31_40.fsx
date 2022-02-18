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

Seq.initInfinite id
|> Seq.filter isPrime
|> Seq.take 10
|> Seq.toList

// problem 32
let gcd a b =
    let rec gcd' a b =
        let c = a % b
        if c = 0 then b else gcd' b c

    if a > b then gcd' a b else gcd' b a

// 1
gcd 13 27

// 2
gcd 20536 7826

// 6
gcd 270 192

// problem 33
let coprime a b = (gcd a b) = 1

// true
coprime 13 27

// true
not (coprime 20536 7826)

// problem 34
let phi n =
    [ 1 .. (n - 1) ]
    |> List.filter (coprime n)
    |> List.length

// 4
phi 10

// 13
phi 13

// problem 35
let factor n =
    let rec div2 n acc =
        if n % 2 <> 0 then
            n, acc
        else
            div2 (n / 2) (2 :: acc)

    let rec factor' n div acc =
        if n = 1 then
            acc |> List.rev
        else if n % div = 0 then
            factor' (n / div) div (div :: acc)
        else
            factor' n (div + 2) acc

    let (n, acc) = div2 n []
    factor' n 3 acc

// [3; 3; 5; 7]
factor 315

// [11 17 23 229]
factor 984929

// problem 36
let factor2 n =
    let rec factor2' xs div count acc =
        match xs with
        | [] -> ((div, count) :: acc) |> List.rev
        | y :: ys ->
            if div = y then
                factor2' ys div (count + 1) acc
            else
                factor2' ys y 1 ((div, count) :: acc)

    let ns = factor n
    factor2' (List.tail ns) (List.head ns) 1 []

// [(3, 2); (5, 1); (7, 1)]
factor2 315

// problem 37
let phiImproved n =
    factor2 n
    |> List.fold
        (fun acc (div, count) ->
            acc
            * (div - 1)
            * (int (System.Math.Pow(float div, float (count - 1)))))
        1
    |> int

// 4
phiImproved 10

// 12
phiImproved 13

// problem 38
let bench f arg =
    let watch = new System.Diagnostics.Stopwatch()
    watch.Start()
    let ret = f arg
    watch.Stop()

    (ret, watch.Elapsed.TotalMilliseconds)

bench (fun n -> phi n) 10090
bench (fun n -> phiImproved n) 10090
