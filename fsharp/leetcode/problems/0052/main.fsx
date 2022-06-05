open System

let totalNQueens (n: int) : int =
    let rec totalNQueens' p n acc =
        if p = n then
            1
        else
            seq { 0 .. (n - 1) }
            |> Seq.fold
                (fun ret m ->
                    let ok =
                        acc
                        |> List.mapi (fun i j -> i + 1, j)
                        |> List.forall (fun (i, j) -> j <> m && Math.Abs(j - m) <> i)

                    if ok then
                        ret + totalNQueens' (p + 1) n (m :: acc)
                    else
                        ret)
                0

    totalNQueens' 0 n []

// 2
totalNQueens 4

// 1
totalNQueens 1

// 92
totalNQueens 8

// 14200
totalNQueens 12
