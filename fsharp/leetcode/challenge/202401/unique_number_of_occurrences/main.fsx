let uniqueOccurences (arr: int list) : bool =
    let rec toFreq arr acc =
        match arr with
        | [] -> acc
        | h :: t ->
            let v = 1 + (Map.tryFind h acc |> Option.defaultValue 0)
            toFreq t (Map.add h (v + 1) acc)

    let rec uniqueOccurences' vs visited =
        match vs with
        | [] -> true
        | h :: t ->
            if Set.contains h visited then
                false
            else
                uniqueOccurences' t (Set.add h visited)

    let freq = toFreq arr Map.empty
    let vs = Map.values freq |> Seq.toList
    uniqueOccurences' vs Set.empty

// true
uniqueOccurences [ 1; 2; 2; 1; 1; 3 ]

// false
uniqueOccurences [ 1; 2 ]

// true
uniqueOccurences [ -3; 0; 1; -3; 1; 1; 1; -3; 10; 0 ]
