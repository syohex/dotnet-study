let minScoreTriangulation (values: int list) : int =
    let rec f left right (values: int[]) cache =
        if left + 2 > right then
            0, cache
        elif left + 2 = right then
            values.[left] * values.[left + 1] * values.[right], cache
        else
            match Map.tryFind (left, right) cache with
            | Some v -> v, cache
            | None ->
                let v, cache =
                    seq { (left + 1) .. (right - 1) }
                    |> Seq.fold
                        (fun (acc, cache) i ->
                            let v = values.[left] * values.[i] * values.[right]
                            let v1, cache1 = f left i values cache
                            let v2, cache2 = f i right values cache1
                            let v = min acc (v + v1 + v2)
                            v, cache2)
                        (System.Int32.MaxValue, cache)

                v, Map.add (left, right) v cache

    let values = Array.ofList values
    f 0 (values.Length - 1) values Map.empty |> fst

// 6
minScoreTriangulation [ 1; 2; 3 ]

// 144
minScoreTriangulation [ 3; 7; 4; 5 ]

// 13
minScoreTriangulation [ 1; 3; 1; 4; 1; 5 ]
