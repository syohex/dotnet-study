let generate (numRows: int) : int list list =
    let rec generate' i limit (prev: int []) acc =
        if i >= limit then
            acc |> List.rev
        else
            let v =
                seq { 0..i }
                |> Seq.fold
                    (fun acc j ->
                        if j = 0 || j = i then
                            1 :: acc
                        else
                            (prev.[j - 1] + prev.[j]) :: acc)
                    []
                |> List.rev

            generate' (i + 1) limit (v |> List.toArray) (v:: acc)

    generate' 0 numRows [||] []


// [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
generate 5

// [[1]]
generate 1
