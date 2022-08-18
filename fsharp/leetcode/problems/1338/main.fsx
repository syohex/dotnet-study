let minSetSize (arr: int list) : int =
    let rec minSetSize' counts i sum half nums =
        match counts with
        | [] -> nums
        | h :: t ->
            let sum' = sum + h

            if sum' >= half then
                i + 1
            else
                minSetSize' t (i + 1) sum' half nums


    let counts =
        arr
        |> List.fold
            (fun acc num ->
                match Map.tryFind num acc with
                | None -> Map.add num 1 acc
                | Some (v) -> Map.add num (v + 1) acc)
            Map.empty
        |> Map.values
        |> Seq.sortWith (fun a b -> compare b a)
        |> Seq.toList

    let half = arr.Length / 2
    let nums = counts.Length
    minSetSize' counts 0 0 half nums

//2
minSetSize [ 3
             3
             3
             3
             5
             5
             5
             2
             2
             7 ]

// 1
minSetSize [ 7; 7; 7; 7; 7; 7 ]
