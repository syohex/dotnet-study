#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let maxAverageRatio (classes: (int * int) list) (extraStudents: int) : double =
    let diff (n: double) (d: double) = (n + 1.0) / (d + 1.0) - n / d

    let rec average q (acc: double) len =
        match PriorityQueue.tryPop q with
        | None -> acc / (double len)
        | Some(((_, n, d), t)) -> average t (acc + n / d) len

    let rec maxAverageRatio' extraStudents q len =
        if extraStudents = 0 then
            average q 0.0 len
        else
            let ((_, n, d), q) = PriorityQueue.pop q
            let q = PriorityQueue.insert (diff (n + 1.0) (d + 1.0), n + 1.0, d + 1.0) q
            maxAverageRatio' (extraStudents - 1) q len

    let len = List.length classes

    let q =
        classes
        |> List.map (fun (n, d) -> double n, double d)
        |> List.fold
            (fun acc (n, d) -> PriorityQueue.insert (diff n d, n, d) acc)
            (PriorityQueue.empty true)

    maxAverageRatio' extraStudents q len

// 0.78333
maxAverageRatio [ (1, 2); (3, 5); (2, 2) ] 2

// 0.53485
maxAverageRatio [ (2, 4); (3, 9); (4, 5); (2, 10) ] 4
