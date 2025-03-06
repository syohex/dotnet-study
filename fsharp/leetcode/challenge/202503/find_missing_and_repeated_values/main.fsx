let findMissingAndRepeatedValues (grid: int[,]) : int * int =
    let len = Array2D.length1 grid

    grid
    |> Seq.cast<int>
    |> Seq.fold
        (fun acc n ->
            let v = Map.tryFind n acc |> Option.defaultValue 0
            Map.add n (v + 1) acc)
        Map.empty
    |> fun m ->
        seq { 1 .. (len * len) }
        |> Seq.fold
            (fun (a, b) i ->
                match Map.tryFind i m with
                | None -> a, i
                | Some v when v = 2 -> i, b
                | _ -> a, b)
            (0, 0)

let grid1 = array2D [ [ 1; 3 ]; [ 2; 2 ] ]
// (2, 4)
findMissingAndRepeatedValues grid1

let grid2 = array2D [ [ 9; 1; 7 ]; [ 8; 9; 2 ]; [ 3; 4; 6 ] ]
// (9, 5)
findMissingAndRepeatedValues grid2
