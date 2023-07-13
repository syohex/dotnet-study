let toGraph prerequisites =
    let rec toGraph' prerequisites acc =
        match prerequisites with
        | [] -> acc
        | (course, parent) :: t ->
            match Map.tryFind parent acc with
            | Some(v) -> toGraph' t (Map.add parent (course :: v) acc)
            | None -> toGraph' t (Map.add parent [ course ] acc)

    toGraph' prerequisites Map.empty

let canFinish (numCourses: int) (prerequisites: (int * int) list) : bool =
    let rec hasCircle graph node passed visited =
        if Set.contains node passed then
            true, visited
        elif Set.contains node visited then
            false, visited
        else
            let passed' = Set.add node passed
            let visited' = Set.add node visited

            match Map.tryFind node graph with
            | None -> false, visited'
            | Some(nodes) ->
                nodes
                |> List.fold
                    (fun (acc, visited) n ->
                        if acc then
                            true, visited
                        else
                            hasCircle graph n passed' visited)
                    (false, visited')

    let graph = toGraph prerequisites

    seq { 0 .. (numCourses - 1) }
    |> Seq.fold
        (fun (circular, visited) node ->
            if circular then
                true, visited
            else
                hasCircle graph node Set.empty visited)
        (false, Set.empty)
    |> fst
    |> not

// true
canFinish 2 [ (1, 0) ]

// false
canFinish 2 [ (1, 0); (0, 1) ]
