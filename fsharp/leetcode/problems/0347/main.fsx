#r "nuget:FSharpx.Collections"

open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type Data =
    { Num: int
      Count: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? Data as o -> this.Num = o.Num && this.Count = o.Count
        | _ -> false

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Data as o ->
                if this.Count <> o.Count then
                    compare this.Count o.Count
                else
                    compare o.Count this.Count
            | _ -> failwith "cannot compare with other types"

let topKFrequent (nums: int list) (k: int) : int list =
    let freq =
        nums
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | Some (v) -> Map.add n (v + 1) acc
                | None -> Map.add n 1 acc)
            Map.empty

    let q =
        freq
        |> Map.fold (fun q k v -> PriorityQueue.insert { Num = k; Count = v } q) (PriorityQueue.empty true)

    let rec popQueues q k acc =
        if k = 0 then
            acc |> List.rev
        else
            let { Num = n; Count = _ }, rest = PriorityQueue.pop q
            popQueues rest (k - 1) (n :: acc)

    popQueues q k []

// [1,2]
topKFrequent [ 1; 1; 1; 2; 2; 3 ] 2

// [1]
topKFrequent [ 1 ] 1
