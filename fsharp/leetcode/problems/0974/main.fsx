let subarraysDivByK (nums: int list) (k: int) : int =
    let rec subarraysDivByK' nums k prefixSum m acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let prefixSum' = (prefixSum + (h % k) + k) % k

            let m', acc' =
                match Map.tryFind prefixSum' m with
                | None -> Map.add prefixSum' 1 m, acc
                | Some(v) -> Map.add prefixSum' (v + 1) m, acc + 1

            subarraysDivByK' t k prefixSum' m' acc'

    let m = Map.empty |> Map.add 0 1
    subarraysDivByK' nums k 0 m 0

// 7
subarraysDivByK [ 4; 5; 0; -2; -3; 1 ] 5

// 0
subarraysDivByK [ 5 ] 9

// 1
subarraysDivByK [ 7; 4; -10 ] 5
