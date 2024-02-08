let numSquares (n: int) : int =
    let rec squares i n acc =
        if i * i >= n then
            List.rev acc
        else
            squares (i + 1) n ((i * i) :: acc)

    let ss = squares 1 n []
    let dp = Array.init (n + 1) (fun _ -> n + 1)
    dp.[0] <- 0

    for i in 1..n do
        ss
        |> List.filter (fun square -> square <= i)
        |> List.iter (fun square -> dp.[i] <- System.Math.Min(dp.[i], dp.[i - square] + 1))

    dp.[n]

// 3
numSquares 12

// 2
numSquares 13
