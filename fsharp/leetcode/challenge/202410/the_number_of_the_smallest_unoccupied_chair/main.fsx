#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let smallestChair (times: (int * int) list) (targetFriend: int) : int =
    let rec smallestChair' data q seats =
        match data with
        | [] -> Map.find targetFriend seats
        | (_, isComing, friend) :: t ->
            if isComing = -1 then
                let seat = Map.find friend seats
                smallestChair' t (PriorityQueue.insert seat q) seats
            else
                let seat, q' = PriorityQueue.pop q
                smallestChair' t q' (Map.add friend seat seats)

    let data =
        times
        |> List.indexed
        |> List.fold (fun acc (i, (come, leave)) -> (leave, -1, i) :: (come, 1, i) :: acc) []
        |> List.sort

    let len = List.length times

    let q =
        seq { 0 .. (len - 1) }
        |> Seq.fold (fun acc i -> PriorityQueue.insert i acc) (PriorityQueue.empty false)

    smallestChair' data q Map.empty

// 1
smallestChair [ (1, 4); (2, 3); (4, 6) ] 1

// 2
smallestChair [ (3, 10); (1, 5); (2, 6) ] 0
