let groupAnagrams (strs: string list) : string list list =
    let rec groupAnagrams' strs acc =
        match strs with
        | [] -> acc |> Map.values |> Seq.toList
        | h :: t ->
            let freq =
                h
                |> Seq.fold
                    (fun acc c ->
                        match Map.tryFind c acc with
                        | Some(v) -> Map.add c (v + 1) acc
                        | None -> Map.add c 1 acc)
                    Map.empty

            match Map.tryFind freq acc with
            | Some(v) -> groupAnagrams' t (Map.add freq (h :: v) acc)
            | None -> groupAnagrams' t (Map.add freq [ h ] acc)

    groupAnagrams' strs Map.empty

// [["bat"],["nat","tan"],["ate","eat","tea"]] in any order
groupAnagrams [ "eat"; "tea"; "tan"; "ate"; "nat"; "bat" ]

// [[""]]
groupAnagrams [ "" ]

// [["a"]]
groupAnagrams [ "a" ]
