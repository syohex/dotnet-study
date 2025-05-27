let differenceOfSums (n: int) (m: int) : int =
    seq { 1..n }
    |> Seq.fold (fun acc v -> if v % m <> 0 then acc + v else acc - v) 0

// 19
differenceOfSums 10 3

// 15
differenceOfSums 5 6

// -15
differenceOfSums 5 1
