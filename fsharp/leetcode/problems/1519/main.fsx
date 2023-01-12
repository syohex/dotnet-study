let edgesToGraph (edges: (int * int) list) : Map<int, int list> =
    let rec edgesToGraph' edges acc =
        match edges with
        | [] -> acc
        | (s, e) :: t ->
            let acc' =
                match Map.tryFind s acc with
                | Some(v) -> Map.add s (e :: v) acc
                | None -> Map.add s [ e ] acc

            let acc'' =
                match Map.tryFind e acc' with
                | Some(v) -> Map.add e (s :: v) acc'
                | None -> Map.add e [ s ] acc'

            edgesToGraph' t acc''

    edgesToGraph' edges Map.empty

let countSubTrees (n: int) (edges: (int * int) list) (labels: string) : int[] =
    let rec countSubTrees' node parent graph (labels: char[]) (ret: int[]) =
        let count =
            Map.find node graph
            |> List.fold
                (fun (acc: int[]) n ->
                    if n = parent then
                        acc
                    else
                        let childCount: int[] = countSubTrees' n node graph labels ret
                        seq { 0..25 } |> Seq.iter (fun i -> acc.[i] <- acc.[i] + childCount.[i])
                        acc)
                (Array.zeroCreate 26)

        let index = int labels.[node] - int 'a'
        count.[index] <- count.[index] + 1
        ret.[node] <- count.[index]
        count

    let graph = edgesToGraph edges
    let labels' = labels |> Seq.toArray
    let ret = Array.zeroCreate n
    countSubTrees' 0 -1 graph labels' ret |> ignore
    ret

let edges1 = [ (0, 1); (0, 2); (1, 4); (1, 5); (2, 3); (2, 6) ]
// [2;1;1;1;1;1;1]
countSubTrees 7 edges1 "abaedcd"

let edges2 = [ (0, 1); (1, 2); (0, 3) ]
// [4;2;1;1]
countSubTrees 4 edges2 "bbbb"

let edge3 = [ (0, 1); (0, 2); (1, 3); (0, 4) ]
// [3;2;1;1;1]
countSubTrees 5 edge3 "aabab"
