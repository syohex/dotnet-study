#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let kSmallestPairs (nums1: int[]) (nums2: int[]) (k: int) : (int * int) list =
    let rec kSmallestPairs' q (nums1: int[]) (nums2: int[]) k visited acc =
        if k = 0 then
            List.rev acc
        else
            match PriorityQueue.tryPop q with
            | None -> List.rev acc
            | Some((_, i, j), t) ->
                let acc' = (nums1.[i], nums2.[j]) :: acc

                let q', visited' =
                    if i + 1 < nums1.Length && (Set.contains (i + 1, j) visited |> not) then
                        PriorityQueue.insert (nums1.[i + 1] + nums2.[j], i + 1, j) t, Set.add (i + 1, j) visited
                    else
                        t, visited

                let q'', visited'' =
                    if j + 1 < nums2.Length && (Set.contains (i, j + 1) visited |> not) then
                        PriorityQueue.insert (nums1.[i] + nums2.[j + 1], i, j + 1) q', Set.add (i, j + 1) visited
                    else
                        q', visited'

                kSmallestPairs' q'' nums1 nums2 (k - 1) visited'' acc'

    let q =
        PriorityQueue.empty false |> PriorityQueue.insert (nums1.[0] + nums2.[0], 0, 0)

    let visited = Set.empty |> Set.add (0, 0)
    kSmallestPairs' q nums1 nums2 k visited []

// [[1,2],[1,4],[1,6]]
kSmallestPairs [| 1; 7; 11 |] [| 2; 4; 6 |] 3

// [[1,1],[1,1]]
kSmallestPairs [| 1; 1; 2 |] [| 1; 2; 3 |] 2

// [[1,3], [2,3]]
kSmallestPairs [| 1; 2 |] [| 3 |] 3
