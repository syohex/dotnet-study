#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

let maxKelements (nums: int list) (k: int) : int64 =
    let q =
        nums
        |> List.map int64
        |> List.fold (fun acc n -> PriorityQueue.insert n acc) (PriorityQueue.empty true)

    seq { 1..k }
    |> Seq.fold
        (fun (ret, q) _ ->
            let n, q' = PriorityQueue.pop q
            let n' = Math.Ceiling(double n / 3.0) |> int64
            ret + n, PriorityQueue.insert n' q')
        (0L, q)
    |> fst

// 50
maxKelements [ 10; 10; 10; 10; 10 ] 5

// 17
maxKelements [ 1; 10; 3; 3; 3 ] 3
