let groupAnagrams (strs: string list) : string list list =
    strs
    |> List.map (fun s ->
        s,
        s
        |> Seq.fold
            (fun (acc: int []) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26))
    |> List.fold
        (fun acc (str, v) ->
            match Map.tryFind v acc with
            | Some (ss) -> Map.add v (str :: ss) acc
            | None -> Map.add v [ str ] acc)
        Map.empty
    |> Map.toList
    |> List.map snd
    |> List.sortWith (fun a b -> compare a.Length b.Length)

groupAnagrams [ "eat"
                "tea"
                "tan"
                "ate"
                "nat"
                "bat" ]

groupAnagrams [ "" ]
groupAnagrams [ "a" ]
