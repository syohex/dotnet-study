#r "nuget:FSharpx.Collections"

open FSharpx.Collections

type NumberContainers =
    { numMap: Map<int, int>
      indexMap: Map<int, IPriorityQueue<int>> }

    static member empty =
        { numMap = Map.empty
          indexMap = Map.empty }

    static member change (index: int) (number: int) (nc: NumberContainers) : NumberContainers =
        let numMap = Map.add index number nc.numMap

        let q =
            Map.tryFind number nc.indexMap
            |> Option.defaultValue (PriorityQueue.empty false)
            |> PriorityQueue.insert index

        { numMap = numMap
          indexMap = Map.add number q nc.indexMap }

    static member find (number: int) (nc: NumberContainers) : int * NumberContainers =
        let rec f q =
            match PriorityQueue.tryPop q with
            | None ->
                -1,
                { nc with
                    indexMap = Map.add number q nc.indexMap }
            | Some((v, q')) ->
                if Map.find v nc.numMap = number then
                    v,
                    { nc with
                        indexMap = Map.add number q nc.indexMap }
                else
                    f q'

        match Map.tryFind number nc.indexMap with
        | None -> -1, nc
        | Some(q) -> f q

// -1
let v, nc = NumberContainers.empty |> NumberContainers.find 10

let nc1 =
    nc
    |> NumberContainers.change 2 10
    |> NumberContainers.change 1 10
    |> NumberContainers.change 3 10
    |> NumberContainers.change 5 10

let v1, nc2 = NumberContainers.find 10 nc1
let nc3 = NumberContainers.change 1 20 nc2
let v2, _ = NumberContainers.find 10 nc3

// -1, 1, 2
printfn $"v={v}, v1={v1} v2={v2}"
