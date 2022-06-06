let arrayChange (nums: int list) (operations: (int * int) list) : int list =
    let rec arrayChange' (nums: int []) operations pos =
        match operations with
        | [] -> nums |> Array.toList
        | (v1, v2) :: rest ->
            match Map.tryFind v1 pos with
            | None -> failwith "never reach here"
            | Some (p) ->
                nums.[p] <- v2
                let pos' = pos |> Map.remove v1 |> Map.add v2 p
                arrayChange' nums rest pos'

    let pos =
        nums
        |> List.mapi (fun i n -> i, n)
        |> List.fold (fun acc (i, n) -> Map.add n i acc) Map.empty

    arrayChange' (nums |> List.toArray) operations pos

// [3;2;7;1]
arrayChange [ 1; 2; 4; 6 ] [
    (1, 3)
    (4, 7)
    (6, 1)
]

// [2,1]
arrayChange [ 1; 2 ] [
    (1, 3)
    (2, 1)
    (3, 2)
]
