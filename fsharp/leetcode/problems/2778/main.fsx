let sumOfSquares (nums: int list) : int =
    let len = List.length nums

    nums
    |> List.indexed
    |> List.filter (fun (i, _) -> len % (i + 1) = 0)
    |> List.map (fun (_, n) -> n * n)
    |> List.sum

// 21
sumOfSquares [ 1; 2; 3; 4 ] |> printfn "%A"

// 63
sumOfSquares [ 2; 7; 1; 19; 18; 3 ] |> printfn "%A"
