#r "nuget: Fsharpx.Collections"

open FSharpx.Collections

let shortestSubarray (nums: int list) (k: int) : int =
    let rec updateShortest q sum i (acc: int64) =
        if PriorityQueue.isEmpty q then
            acc, q
        else
            let (s, j) = PriorityQueue.peek q

            if sum - s >= k then
                let _, q = PriorityQueue.pop q
                updateShortest q sum i (min acc (int64 (i - j)))
            else
                acc, q

    let rec shortestSubarray' nums q sum (acc: int64) =
        match nums with
        | [] -> if acc = System.Int64.MaxValue then -1 else int acc
        | (i, h) :: t ->
            let sum = sum + h
            let acc, q = updateShortest q sum i acc
            shortestSubarray' t (PriorityQueue.insert (sum, i) q) sum acc

    let q = PriorityQueue.empty false |> PriorityQueue.insert (0, -1)
    shortestSubarray' (List.indexed nums) q 0 System.Int64.MaxValue

// 1
shortestSubarray [ 1 ] 1

// -1
shortestSubarray [ 1; 2 ] 4

// 3
shortestSubarray [ 2; -1; 2 ] 3
