let beautifulSubsets (nums: int list) (k: int) =
    let rec beautifulSubsets' nums k freq =
        match nums with
        | [] -> if Map.isEmpty freq then 0 else 1
        | h :: t ->
            let ret = beautifulSubsets' t k freq
            let v = Map.tryFind (h - k) freq |> Option.defaultValue 0

            if v = 0 then
                let w = Map.tryFind h freq |> Option.defaultValue 0
                ret + beautifulSubsets' t k (Map.add h (w + 1) freq)
            else
                ret

    beautifulSubsets' (List.sort nums) k Map.empty

// 4
beautifulSubsets [ 2; 4; 6 ] 2

// 1
beautifulSubsets [ 1 ] 1
