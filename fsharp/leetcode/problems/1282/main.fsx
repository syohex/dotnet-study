let groupThePeople (groupSizes: int list) : int list list =
    groupSizes
    |> List.indexed
    |> List.fold
        (fun acc (i, group) ->
            match Map.tryFind group acc with
            | Some(v) -> Map.add group (i :: v) acc
            | None -> Map.add group [ i ] acc)
        Map.empty
    |> Map.fold
        (fun acc k v ->
            let len = List.length v
            let chunks = List.splitInto (len / k) (List.rev v)
            chunks @ acc)
        []
    |> List.rev


// [[5],[0,1,2],[3,4,6]] in any order
groupThePeople [ 3; 3; 3; 3; 3; 1; 3 ]

// [[1],[0,5],[2,3,4]] in any order
groupThePeople [ 2; 1; 3; 3; 3; 2 ]
