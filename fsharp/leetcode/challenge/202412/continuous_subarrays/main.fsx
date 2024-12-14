#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let continuousSubarray (nums: int list) : int64 =
    let rec updateQueue index q = 
        match PriorityQueue.tryPop q with
        | None -> q
        | Some(((_, i), q')) ->
            if i < index then
                updateQueue index q'
            else
                q

    let rec updateWindow left right minQ maxQ =
        if left >= right then
            left, minQ, maxQ
        else
            let min, _ = PriorityQueue.peek minQ
            let max, _ = PriorityQueue.peek maxQ

            if max - min <= 2 then
                left, minQ, maxQ
            else
                let left = left + 1
                let minQ = updateQueue left minQ
                let maxQ = updateQueue left maxQ
                updateWindow left right minQ maxQ

    let rec continuousSubarray' nums left minQ maxQ acc =
        match nums with
        | [] -> acc
        | (right, h) :: t ->
            let minQ = PriorityQueue.insert (h, right) minQ
            let maxQ = PriorityQueue.insert (h, right) maxQ

            let left, minQ, maxQ = updateWindow left right minQ maxQ
            let acc = acc + (int64 <| right - left + 1)
            continuousSubarray' t left minQ maxQ acc

    let nums = nums |> List.indexed
    let minQ = PriorityQueue.empty false
    let maxQ = PriorityQueue.empty true
    continuousSubarray' nums 0 minQ maxQ 0L

// 8
continuousSubarray [ 5; 4; 2; 4 ]

// 6
continuousSubarray [ 1..3 ]

// 39
continuousSubarray [ 35; 35; 36; 37; 36; 37; 38; 37; 38 ]

// 28
continuousSubarray [ 42; 41; 42; 41; 41; 40; 39; 38 ]

// 43
continuousSubarray [ 65; 66; 67; 66; 66; 65; 64; 65; 65; 64 ]
