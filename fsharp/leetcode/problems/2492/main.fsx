open System

let roadsToGraph (n: int) (roads: (int * int * int) list) : ((int * int) list)[] =
    let rec roadsToGraph' roads (acc: ((int * int) list)[]) =
        match roads with
        | [] -> acc
        | (a, b, distance) :: t ->
            acc.[a] <- ((b, distance) :: acc.[a])
            acc.[b] <- ((a, distance) :: acc.[b])
            roadsToGraph' t acc

    roadsToGraph' roads (Array.init (n + 1) (fun _ -> []))

let minScore (n: int) (roads: (int * int * int) list) : int =
    let rec minScore' q (graph: ((int * int) list)[]) (scores: int[]) ret =
        match q with
        | [] ->
            ret
        | node :: t ->
            let q', ret' =
                graph.[node]
                |> List.fold
                    (fun (q', ret') (next, score) ->
                        if score >= scores.[next] then
                            q', ret'
                        else
                            scores.[next] <- score
                            next :: q', Math.Min(ret', score))
                    (t, ret)
            minScore' q' graph scores ret'

    let graph = roadsToGraph n roads
    minScore' [ 1 ] graph (Array.init (n + 1) (fun _ -> Int32.MaxValue)) Int32.MaxValue

let roads1 = [ (1, 2, 9); (2, 3, 6); (2, 4, 5); (1, 4, 7) ]
// 5
minScore 4 roads1

let roads2 = [ (1, 2, 2); (1, 3, 4); (3, 4, 7) ]
// 2
minScore 4 roads2
