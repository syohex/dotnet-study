let applyOperations (nums: int list) : int list =
    let rec applyOperations' nums acc =
        match nums with
        | [] ->
            let nonZeros = acc |> List.filter (fun n -> n <> 0)
            let zeros = acc.Length - nonZeros.Length

            seq { 1..zeros }
            |> Seq.fold (fun acc' _ -> 0 :: acc') nonZeros
            |> List.rev
        | h :: t ->
            match acc with
            | [] -> applyOperations' t [ h ]
            | prev :: rest ->
                if prev = h then
                    applyOperations' t (0 :: (2 * prev) :: rest)
                else
                    applyOperations' t (h :: acc)

    applyOperations' nums []

// [1,4,2,0,0,0]
applyOperations [ 1; 2; 2; 1; 1; 0 ]

// [0,1]
applyOperations [ 1; 0 ]
