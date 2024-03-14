let numSubarraysWithSum (nums: int list) (goal: int) : int =
    let rec numSubarraysWithSum' nums sum goal m ret =
        match nums with
        | [] -> ret
        | h :: t ->
            let sum' = sum + h
            let ret' = if sum' = goal then ret + 1 else ret
            let diff = sum' - goal

            let m' =
                match Map.tryFind sum' m with
                | Some(v) -> Map.add sum' (v + 1) m
                | None -> Map.add sum' 1 m

            match Map.tryFind diff m with
            | Some(v) -> numSubarraysWithSum' t sum' goal m' (ret' + v)
            | None -> numSubarraysWithSum' t sum' goal m' ret'

    numSubarraysWithSum' nums 0 goal Map.empty 0

// 4
numSubarraysWithSum [ 1; 0; 1; 0; 1 ] 2

// 15
numSubarraysWithSum [ 0; 0; 0; 0; 0 ] 0
