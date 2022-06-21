#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let furthestBuilding (heights: int list) (bricks: int) (ladders: int) : int =
    let rec furthestBuilding' (q: IPriorityQueue<int>) qLen prev i heights bricks ladders =
        match heights with
        | [] -> i
        | h :: t ->
            let diff = h - prev

            if diff <= 0 then
                furthestBuilding' q qLen h (i + 1) t bricks ladders
            else
                let q' = PriorityQueue.insert diff q
                let qLen' = qLen + 1

                if qLen' <= ladders then
                    furthestBuilding' q' qLen' h (i + 1) t bricks ladders
                else
                    let min, q'' = PriorityQueue.pop q'
                    let bricks' = bricks - min

                    if bricks' < 0 then
                        i
                    else
                        furthestBuilding' q'' qLen h (i + 1) t bricks' ladders

    let q = PriorityQueue.empty false
    furthestBuilding' q 0 heights.Head 0 heights.Tail bricks ladders

// 4
furthestBuilding [ 4; 2; 7; 6; 9; 14; 12 ] 5 1

// 7
furthestBuilding [ 4; 12; 2; 7; 3; 18; 20; 3; 19 ] 10 2

// 3
furthestBuilding [ 14; 3; 19; 3 ] 17 0

// 3
furthestBuilding [ 4; 3; 2; 1 ] 0 0

// 1
furthestBuilding [ 7; 5; 13 ] 0 0
