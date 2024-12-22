#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let leftMostBuildingQueries (heights: int list) (queries: (int * int) list) : int list =
    let rec tryToResolve i height q (ret: int[]) =
        match PriorityQueue.tryPop q with
        | None -> q, ret
        | Some(((h, queryIndex), q')) ->
            if h >= height then
                q, ret
            else
                ret.[queryIndex] <- i
                tryToResolve i height q' ret

    let updateQueue i q unresolved =
        match Map.tryFind i unresolved with
        | None -> q
        | Some(v) ->
            v
            |> List.fold (fun q (h, queryIndex) -> PriorityQueue.insert (h, queryIndex) q) q

    let rec leftMostBuildingQueries' heights unresolved q (ret: int[]) =
        match heights with
        | [] -> ret |> Array.toList
        | (i, h) :: t ->
            let q, ret = tryToResolve i h q ret
            let q = updateQueue i q unresolved
            leftMostBuildingQueries' t unresolved q ret


    let heightArray = Array.ofList heights
    let ret = Array.init (List.length queries) (fun _ -> -1)

    let (ret, unresolved) =
        queries
        |> List.indexed
        |> List.fold
            (fun ((ret: int[]), unresolved) (i, (a, b)) ->
                let a, b = min a b, max a b

                if a = b || heightArray.[a] < heightArray.[b] then
                    ret.[i] <- b
                    ret, unresolved
                else
                    let h = max heightArray.[a] heightArray.[b]

                    let unresolved =
                        match Map.tryFind b unresolved with
                        | Some(v) -> Map.add b ((h, i) :: v) unresolved
                        | None -> Map.add b [ (h, i) ] unresolved

                    ret, unresolved)
            (ret, Map.empty)

    let heights = List.indexed heights
    let q = PriorityQueue.empty false
    leftMostBuildingQueries' heights unresolved q ret

// [2,5,-1,5,2]
leftMostBuildingQueries [ 6; 4; 8; 5; 2; 7 ] [ (0, 1); (0, 3); (2, 4); (3, 4); (2, 2) ]
// [7,6,-1,4,6]
leftMostBuildingQueries [ 5; 3; 8; 2; 6; 1; 4; 6 ] [ (0, 7); (3, 5); (5, 2); (3, 0); (1, 6) ]
