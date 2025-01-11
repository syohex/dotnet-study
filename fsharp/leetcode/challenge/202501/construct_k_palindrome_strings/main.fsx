let canConstruct (s: string) (k: int) : bool =
    if s.Length < k then
        false
    else
        let freq =
            s
            |> Seq.fold
                (fun acc c ->
                    let v = Map.tryFind c acc |> Option.defaultValue 0
                    Map.add c (v + 1) acc)
                Map.empty

        let odds = freq |> Map.fold (fun acc _ v -> acc + if v % 2 = 1 then 1 else 0) 0
        odds <= k

// true
canConstruct "annabelle" 2

// false
canConstruct "leetcode" 3

// true
canConstruct "true" 4

// false
canConstruct "cr" 99
