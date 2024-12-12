#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let pickGifts (gifts: int list) (k: int) : int64 =
    let rec sum q acc =
        match PriorityQueue.tryPop q with
        | Some((v, q)) -> sum q (acc + int64 v)
        | None -> acc

    let q =
        gifts
        |> List.fold (fun acc n -> PriorityQueue.insert n acc) (PriorityQueue.empty true)

    let squareRoot = double >> sqrt >> int

    let q =
        seq { 1..k }
        |> Seq.fold
            (fun acc _ ->
                let v, acc = PriorityQueue.pop acc
                PriorityQueue.insert (squareRoot v) acc)
            q

    sum q 0

// 29
pickGifts [ 25; 64; 9; 4; 100 ] 4

// 4
pickGifts [ 1; 1; 1; 1 ] 4
