let change (amount: int) (coins: int list) : int =
    let rec change' (dp: int[]) amount coins =
        match coins with
        | [] -> dp.[amount]
        | h :: t ->
            seq { h..amount } |> Seq.iter (fun i -> dp.[i] <- dp.[i] + dp.[i - h])

            change' dp amount t


    let dp = Array.zeroCreate (amount + 1)
    dp.[0] <- 1
    change' dp amount coins

// 4
change 5 [ 1; 2; 5 ]

// 0
change 3 [ 2 ]

// 1
change 10 [ 10 ]
