let combine (n: int) (k: int) : int list list =
    let rec combine' start n k acc len ret =
        if len >= k then
            (List.rev acc) :: ret
        else
            seq { start..n }
            |> Seq.fold (fun ret i -> combine' (i + 1) n k (i :: acc) (len + 1) ret) ret

    combine' 1 n k [] 0 []

// [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]] in any order
combine 4 2 |> List.rev

// [[1]]
combine 1 1

combine 5 3 |> List.rev
