let cumulativeList (nums: int list) =
    let rec cumulativeList' nums acc =
        match nums with
        | [] -> acc |> List.rev |> List.tail
        | head :: tail ->
            let v = head + (List.head acc)
            cumulativeList' tail (v :: acc)

    cumulativeList' nums [ 0 ]

let subarraySum (nums: int list) (k: int) : int =
    let rec subarraySum' cums k m acc =
        match cums with
        | [] -> acc
        | head :: tail ->
            match Map.tryFind (head - k) m with
            | None ->
                match Map.tryFind head m with
                | None -> subarraySum' tail k (Map.add head 1 m) acc
                | Some w -> subarraySum' tail k (Map.add head (w + 1) m) acc
            | Some v ->
                match Map.tryFind head m with
                | None -> subarraySum' tail k (Map.add head 1 m) (acc + v)
                | Some w -> subarraySum' tail k (Map.add head (w + 1) m) (acc + v)

    let cums = cumulativeList nums
    let m = Map.empty |> Map.add 0 1
    subarraySum' cums k m 0

// 2
subarraySum [ 1; 1; 1 ] 2

// 2
subarraySum [ 1; 2; 3 ] 3
