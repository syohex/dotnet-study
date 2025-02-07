let queryResults (limit: int) (queries: (int * int) list) : int list =
    let rec queryResults' queries (balls: int[]) colors acc =
        match queries with
        | [] -> List.rev acc
        | (id, color) :: t ->
            let oldColor = balls.[id]
            balls.[id] <- color

            let colors =
                if oldColor = 0 then
                    colors
                else
                    let n = Map.find oldColor colors

                    if n = 0 then
                        Map.remove oldColor colors
                    else
                        Map.add oldColor (n - 1) colors

            let n = Map.tryFind color colors |> Option.defaultValue 0
            let colors = Map.add color (n + 1) colors
            queryResults' t balls colors ((Map.count colors) :: acc)

    queryResults' queries (Array.zeroCreate (limit + 1)) Map.empty []

// [1,2,2,3]
queryResults 4 [ (1, 4); (2, 5); (1, 3); (3, 4) ]

// [1,2,2,3,4]
queryResults 4 [ (0, 1); (1, 2); (2, 2); (3, 4); (4, 5) ]
