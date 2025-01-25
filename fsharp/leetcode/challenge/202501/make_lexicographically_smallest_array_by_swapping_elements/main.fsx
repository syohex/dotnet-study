let lexicographicallySmallestArray (nums: int list) (limit: int) : int list =
    let rec toGroup nums prev groupId groupMap groups =
        match nums with
        | [] -> groupMap, Map.map (fun _ v -> List.sort v) groups
        | h :: t ->
            let groupId = if h - prev > limit then groupId + 1 else groupId
            let groupMap = Map.add h groupId groupMap
            let v = Map.tryFind groupId groups |> Option.defaultValue []
            let groups = Map.add groupId (h :: v) groups
            toGroup t h groupId groupMap groups

    let rec f nums groupMap groups acc =
        match nums with
        | [] -> List.rev acc
        | h :: t ->
            let groupId = Map.find h groupMap
            let vs = Map.find groupId groups
            f t groupMap (Map.add groupId (List.tail vs) groups) ((List.head vs) :: acc)

    let sorted = List.sort nums
    let groupMap, groups = toGroup sorted (List.head sorted) 0 Map.empty Map.empty
    f nums groupMap groups []

// [1,3,5,8,9]
lexicographicallySmallestArray [ 1; 5; 3; 9; 8 ] 2

// [1,6,7,18,1,2]
lexicographicallySmallestArray [ 1; 7; 6; 18; 2; 1 ] 3

// [1,7,28,19,10]
lexicographicallySmallestArray [ 1; 7; 28; 19; 10 ] 3
