let generate (numRows: int) : int[] list =
    let rec generate' i (prev: int[]) acc =
        if i = numRows then
            List.rev acc
        else
            let v =
                seq { 0..i }
                |> Seq.map (fun j -> if j = 0 || j = i then 1 else prev.[j - 1] + prev.[j])
                |> Seq.toArray

            generate' (i + 1) v (v :: acc)

    generate' 1 [| 1 |] [ [| 1 |] ]

generate 5

generate 1
