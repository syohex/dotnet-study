let mostFrequentEven (nums: int list) : int =
    let freq =
        nums
        |> List.filter (fun n -> n % 2 = 0)
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | None -> Map.add n 1 acc
                | Some (v) -> Map.add n (v + 1) acc)
            Map.empty

    freq
    |> Map.fold
        (fun (ret, max) k v ->
            if v > max || (v = max && k < ret) then
                k, v
            else
                ret, max)
        (-1, 0)
    |> fst

// 2
mostFrequentEven [ 0; 1; 2; 2; 4; 4; 1 ]

// 4
mostFrequentEven [ 4; 4; 4; 9; 2; 4 ]

// -1
mostFrequentEven [ 29
                   47
                   21
                   41
                   13
                   37
                   25
                   7 ]