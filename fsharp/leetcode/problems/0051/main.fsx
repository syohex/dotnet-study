open System

let stringLine (p: int) (n: int) : string =
    let rec stringLine' i p n (acc: char list) =
        if i >= n then
            acc |> List.rev |> String.Concat
        elif i = p then
            stringLine' (i + 1) p n ('Q' :: acc)
        else
            stringLine' (i + 1) p n ('.' :: acc)

    stringLine' 0 p n []


let solveNQueens (n: int) : string list list =
    let check (p: int) acc =
        let ok1 = acc |> List.forall (fun n -> p <> n)

        let ok2 =
            acc
            |> List.mapi (fun i m -> i + 1, m)
            |> List.forall (fun (i, m) -> Math.Abs(p - m) <> i)

        ok1 && ok2

    let rec solveNQueens' i n acc ret =
        if i >= n then
            let lines =
                acc
                |> List.rev
                |> List.map (fun p -> stringLine p n)

            lines :: ret
        else
            seq { 0 .. (n - 1) }
            |> Seq.fold
                (fun ret m ->
                    if check m acc then
                        let ret' = solveNQueens' (i + 1) n (m :: acc) ret
                        ret'
                    else
                        ret)
                ret

    solveNQueens' 0 n [] []

// [[".Q..","...Q","Q...","..Q."],["..Q.","Q...","...Q",".Q.."]]
solveNQueens 4

// 724
solveNQueens 10 |> List.length
