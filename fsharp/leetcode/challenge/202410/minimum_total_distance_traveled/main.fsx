let minimumTotalDistance (robot: int list) (factory: (int * int) list) : int64 =
    let rec minimumTotalDistance' robot factory cache =
        match robot, factory with
        | [], _ -> 0L, cache
        | _, [] -> pown 2L 40, cache
        | (i, r) :: t1, (j, f) :: t2 ->
            match Map.tryFind (i, j) cache with
            | Some(v) -> v, cache
            | None ->
                let v1, cache = minimumTotalDistance' t1 t2 cache
                let ret1 = (abs (r - f)) + v1
                let ret2, cache = minimumTotalDistance' robot t2 cache
                let ret = min ret1 ret2
                ret, Map.add (i, j) ret cache

    let robot = robot |> List.map int64 |> List.sort |> List.indexed

    let factory =
        factory
        |> List.fold (fun acc (pos, count) -> List.replicate count (int64 pos) @ acc) []
        |> List.sort
        |> List.indexed

    minimumTotalDistance' robot factory Map.empty |> fst

// 4
minimumTotalDistance [ 0; 4; 6 ] [ (2, 2); (6, 2) ]

// 2
minimumTotalDistance [ 1; -1 ] [ (-2, 1); (2, 1) ]
