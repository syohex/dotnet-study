let xorQueries (arr: int list) (queries: (int * int) list) : int list =
    let rec xorQueries' queries (arr: int[]) acc =
        match queries with
        | [] -> List.rev acc
        | (s1, e1) :: t -> xorQueries' t arr ((arr.[e1 + 1] ^^^ arr.[s1]) :: acc)

    let arr =
        arr
        |> List.fold (fun acc n -> ((List.head acc) ^^^ n) :: acc) [ 0 ]
        |> List.rev
        |> List.toArray

    xorQueries' queries arr []

// [2,7,14,8]
xorQueries [ 1; 3; 4; 8 ] [ (0, 1); (1, 2); (0, 3); (3, 3) ]

// [8,0,4,4]
xorQueries [ 4; 8; 2; 10 ] [ (2, 3); (1, 3); (0, 0); (0, 3) ]
