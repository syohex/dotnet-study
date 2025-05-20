let isZeroArray (nums: int list) (queries: (int * int) list) : bool =
    let rec isZeroArray' nums vals =
        match nums, vals with
        | [], [] -> true
        | [], _
        | _, [] -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 -> if h1 > h2 then false else isZeroArray' t1 t2

    let len = nums.Length

    let diffs =
        queries
        |> List.fold
            (fun (acc: int[]) (s, e) ->
                acc.[s] <- acc.[s] + 1
                acc.[e + 1] <- acc.[e + 1] - 1
                acc)
            (Array.zeroCreate (len + 1))
        |> Array.take len

    let vals =
        diffs
        |> Array.fold
            (fun (acc, v) n ->
                let v = v + n
                v :: acc, v)
            ([], 0)
        |> fst
        |> List.rev

    isZeroArray' nums vals

// true
isZeroArray [ 1; 0; 1 ] [ (0, 2) ]

// false
isZeroArray [ 4; 3; 2; 1 ] [ (1, 3); (0, 2) ]
