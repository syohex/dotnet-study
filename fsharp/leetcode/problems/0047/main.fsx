let permuteUnique (nums: int list) : int list list =
    let rec permuteUnique' freq limit len acc =
        if len = limit then
            [ acc |> List.rev ]
        else
            freq
            |> Map.fold
                (fun ret k v ->
                    if v = 0 then
                        ret
                    else
                        let r =
                            permuteUnique' (Map.add k (v - 1) freq) limit (len + 1) (k :: acc)

                        r @ ret)
                []

    let freq =
        nums
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | None -> Map.add n 1 acc
                | Some (v) -> Map.add n (v + 1) acc)
            Map.empty

    permuteUnique' freq nums.Length 0 [] |> List.rev

// [[1,1,2], [1,2,1], [2,1,1]]
permuteUnique [ 1; 1; 2 ]

// [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permuteUnique [ 1; 2; 3 ]
