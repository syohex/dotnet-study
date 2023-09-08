let generate (numRows: int) : int list list =
    let rec generate' i numRows (prev: int[]) acc =
        if i >= numRows then
            List.rev acc
        else
            let v =
                seq { 0..i }
                |> Seq.fold
                    (fun acc n ->
                        if n = 0 then prev.[0] :: acc
                        elif n = i then prev.[n - 1] :: acc
                        else (prev.[n - 1] + prev.[n]) :: acc)
                    []
                |> List.rev

            generate' (i + 1) numRows (List.toArray v) (v :: acc)

    generate' 1 numRows [| 1 |] [ [ 1 ] ]

generate 2

// [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
generate 5

// [[1]]
generate 1

generate 7
