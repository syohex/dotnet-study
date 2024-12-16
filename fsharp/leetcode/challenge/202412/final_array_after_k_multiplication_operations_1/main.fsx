#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let getFinalState (nums: int list) (k: int) (multiplier: int) : int list =
    let len = List.length nums

    let q =
        nums
        |> List.indexed
        |> List.fold (fun acc (i, n) -> PriorityQueue.insert (n, i) acc) (PriorityQueue.empty false)

    let q =
        seq { 1..k }
        |> Seq.fold
            (fun acc _ ->
                let (n, i), q = PriorityQueue.pop acc
                PriorityQueue.insert (n * multiplier, i) q)
            q

    seq { 1..len }
    |> Seq.fold
        (fun (acc, q) _ ->
            let (n, i), q = PriorityQueue.pop q
            (i, n) :: acc, q)
        ([], q)
    |> fst
    |> List.sortBy fst
    |> List.map snd

// [8,4,6,5,6]
getFinalState [ 2; 1; 3; 5; 6 ] 5 2

// [16, 8]
getFinalState [ 1; 2 ] 3 4
