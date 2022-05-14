#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type Data =
    { Node: int
      Cost: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? Data as o -> this.Node = o.Node && this.Cost = o.Cost
        | _ -> failwith "cannot compare with other types"

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Data as o -> compare this.Cost o.Cost
            | _ -> failwith "cannot compare with other types"

let timesToGraph (times: (int * int * int) list) : Map<int, (int * int) list> =
    times
    |> List.fold
        (fun acc (node, next, cost) ->
            match Map.tryFind node acc with
            | Some (v) -> Map.add node ((next, cost) :: v) acc
            | None -> Map.add node [ (next, cost) ] acc)
        Map.empty

let rec networkDelayTime'
    (q: IPriorityQueue<Data>)
    (graph: Map<int, (int * int) list>)
    (visited: Set<int>)
    (costs: int [])
    : unit =
    match PriorityQueue.tryPop q with
    | None -> ()
    | Some ((d, rest)) ->
        let visited' = Set.add d.Node visited
        costs.[d.Node-1] <- Math.Min(costs.[d.Node-1], d.Cost)

        match Map.tryFind d.Node graph with
        | None -> networkDelayTime' rest graph visited' costs
        | Some (nexts) ->
            let q' =
                nexts
                |> List.filter (fun (next, _) -> Set.contains next visited' |> not)
                |> List.fold
                    (fun acc (next, cost) -> PriorityQueue.insert { Node = next; Cost = d.Cost + cost } acc)
                    rest

            networkDelayTime' q' graph visited' costs


let networkDelayTime (times: (int * int * int) list) (n: int) (k: int) : int =
    let graph = timesToGraph times

    let q =
        PriorityQueue.empty false
        |> PriorityQueue.insert { Node = k; Cost = 0 }

    let costs = Array.init n (fun _ -> Int32.MaxValue)
    networkDelayTime' q graph Set.empty costs

    match Array.tryFind ((=) Int32.MaxValue) costs with
    | Some (_) -> -1
    | None -> Array.max costs

// 2
networkDelayTime [ (2, 1, 1); (2, 3, 1); (3, 4, 1) ] 4 2

// 1
networkDelayTime [ (1, 2, 1) ] 2 1

// -1
networkDelayTime [ (1, 2, 1) ] 2 2

// 3
networkDelayTime [ (1, 2, 1); (2, 3, 2); (1, 3, 4) ] 3 1
