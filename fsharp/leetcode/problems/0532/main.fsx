let toMap (nums: int list) : Map<int, int> =
    let rec toMap' nums m =
        match nums with
        | [] -> m
        | head :: tail ->
            match Map.tryFind head m with
            | Some v -> toMap' tail (Map.add head (v + 1) m)
            | None -> toMap' tail (Map.add head 1 m)

    toMap' nums Map.empty

let findPairs (nums: int list) (k: int) : int =
    let rec findPairs' keys k m acc =
        match keys with
        | [] -> acc
        | head :: tail ->
            match Map.tryFind (head - k) m with
            | None -> findPairs' tail k m acc
            | Some v ->
                if k = 0 then
                    if v >= 2 then
                        findPairs' tail k m (acc + 1)
                    else
                        findPairs' tail k m acc
                else
                    findPairs' tail k m (acc + 1)

    let m = toMap nums
    findPairs' (m |> Map.keys |> Seq.toList) k m 0

// 2
findPairs [ 3; 1; 4; 1; 5 ] 2

// 4
findPairs [ 1; 2; 3; 4; 5 ] 1

// 1
findPairs [ 1; 3; 1; 5; 4 ] 0
