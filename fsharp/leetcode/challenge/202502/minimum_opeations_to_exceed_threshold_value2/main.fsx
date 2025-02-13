#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minOperations (nums: int list) (k: int) : int =
    let rec minOperations' (q: IPriorityQueue<int64>) k steps =
        match PriorityQueue.tryPeek q with
        | None -> failwith "never reach here"
        | Some v when v >= k -> steps
        | _ ->
            let x, q' = PriorityQueue.pop q

            match PriorityQueue.tryPop q' with
            | None -> failwith "never reach here"
            | Some((y, q'')) ->
                let v = (min x y) * 2L + max x y
                let q = PriorityQueue.insert v q''
                minOperations' q k (steps + 1)


    let q =
        nums
        |> List.fold (fun acc n -> PriorityQueue.insert (int64 n) acc) (PriorityQueue.empty false)

    minOperations' q (int64 k) 0

// 2
minOperations [ 2; 11; 10; 1; 3 ] 10

// 4
minOperations [ 1; 1; 2; 4; 9 ] 20

// 2
minOperations [ 1000000000; 999999999; 1000000000; 999999999; 999999999 ] 1000000000
