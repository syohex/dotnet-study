#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let halveArray (nums: int list) : int =
    let rec halveArray' (q: IPriorityQueue<double>) count (sum: double) half =
        if sum <= half then
            count
        else
            let (v, rest) = PriorityQueue.pop q
            let newSum = sum - (v / 2.0)
            let newQ = PriorityQueue.insert (v / 2.0) rest
            halveArray' newQ (count + 1) newSum half

    let q =
        nums
        |> List.fold (fun q n -> PriorityQueue.insert (double n) q) (PriorityQueue.empty true)

    let sum =
        nums
        |> List.fold (fun acc a -> acc + (double a)) 0.0

    halveArray' q 0 sum (sum / 2.0)

// 3
halveArray [ 5; 19; 8; 1 ]

// 3
halveArray [ 3; 8; 20 ]

// 1
halveArray [ 1 ]
