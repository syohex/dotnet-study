#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let maxTwoEvent (events: (int * int * int) list) : int =
    let rec updateMaxValue start q maxValue =
        match PriorityQueue.tryPeek q with
        | None -> q, maxValue
        | Some((end2, value2)) ->
            if start <= end2 then
                q, maxValue
            else
                let _, q = PriorityQueue.pop q
                updateMaxValue start q (max maxValue value2)

    let rec maxTwoEvent' events q maxVal acc =
        match events with
        | [] -> acc
        | (start, end_, value) :: t ->
            let q, maxVal = updateMaxValue start q maxVal
            let acc = max acc (maxVal + value)
            maxTwoEvent' t (PriorityQueue.insert (end_, value) q) maxVal acc

    maxTwoEvent' (List.sort events) (PriorityQueue.empty false) 0 0

// 4
maxTwoEvent [ (1, 3, 2); (4, 5, 2); (2, 4, 3) ]
// 5
maxTwoEvent [ (1, 3, 2); (4, 5, 2); (1, 5, 5) ]
// 8
maxTwoEvent [ (1, 5, 3); (1, 5, 1); (6, 6, 5) ]
