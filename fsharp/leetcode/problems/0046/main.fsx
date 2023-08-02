let permute (nums: int list) : int list list =
    let rec permute' (nums: int[]) used acc len ret =
        if len = nums.Length then
            (List.rev acc) :: ret
        else
            nums
            |> Array.fold
                (fun ret n ->
                    if Set.contains n used then
                        ret
                    else
                        permute' nums (Set.add n used) (n :: acc) (len + 1) ret)
                ret

    permute' (Array.ofList nums) Set.empty [] 0 []

// [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permute [ 1; 2; 3 ] |> List.rev

// [[0, 1], [1, 0]]
permute [ 0; 1 ] |> List.rev

// [[1]]
permute [ 1 ]

let rec permute2 (nums: int list) : int list list =
    let rec interleave n ns =
        match ns with
        | [] -> [ [ n ] ]
        | h :: t -> (n :: h :: t) :: (List.map (fun ns -> h :: ns) (interleave n t))

    match nums with
    | [] -> []
    | h :: [] -> [ [ h ] ]
    | h :: t -> permute2 t |> List.fold (fun acc ns -> (interleave h ns) @ acc) []

permute2 [ 1; 2; 3 ]
permute2 [ 0; 1 ]
permute2 [ 1 ]
