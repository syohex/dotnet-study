#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minStoneSum (piles: int list) (k: int) : int =
    let rec sumQueue (q: IPriorityQueue<int>) acc : int =
        match (PriorityQueue.tryPop q) with
        | None -> acc
        | Some(v, q') -> sumQueue q' (v + acc)

    let rec minStoneSum' (q: IPriorityQueue<int>) (k: int) =
        if k = 0 then
            sumQueue q 0
        else
            let v, q' = PriorityQueue.pop q
            let q'' = PriorityQueue.insert (v - (v / 2)) q'
            minStoneSum' q'' (k - 1)

    let q =
        piles
        |> List.fold (fun acc n -> PriorityQueue.insert n acc) (PriorityQueue.empty true)

    minStoneSum' q k

// 12
minStoneSum [ 5; 4; 9 ] 2

// 12
minStoneSum [ 4; 3; 6; 7 ] 3
