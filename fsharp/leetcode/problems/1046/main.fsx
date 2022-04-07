#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let lastStoneWeight (stones: int list) : int =
    let rec lastStoneWeight' q =
        if PriorityQueue.isEmpty q then
            0
        else
            let first, rest = PriorityQueue.pop q

            if PriorityQueue.isEmpty rest then
                first
            else
                let second, rest' = PriorityQueue.pop rest

                if first = second then
                    lastStoneWeight' rest'
                else
                    lastStoneWeight' (rest' |> PriorityQueue.insert (first - second))

    let q =
        stones
        |> List.fold (fun q n -> PriorityQueue.insert n q) (PriorityQueue.empty true)

    lastStoneWeight' q

// 1
lastStoneWeight [ 2; 7; 4; 1; 8; 1 ]

// 1
lastStoneWeight [ 1 ]

// 0
lastStoneWeight [ 2; 2 ]
