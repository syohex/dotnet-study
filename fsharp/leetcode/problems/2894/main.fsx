let differenceOfSums (n: int) (m: int) : int =
    let num1 = seq { 1..n } |> Seq.filter (fun i -> i % m <> 0) |> Seq.sum
    let num2 = seq { 1..n } |> Seq.filter (fun i -> i % m = 0) |> Seq.sum

    num1 - num2

// 19
differenceOfSums 10 3

// 15
differenceOfSums 5 6

// -15
differenceOfSums 5 1
