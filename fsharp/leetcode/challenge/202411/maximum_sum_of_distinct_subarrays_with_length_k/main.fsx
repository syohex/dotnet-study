let maximumSubarraySum (nums: int[]) (k: int) : int64 =
    let rec maximumSubarraySum' i (nums: int64[]) (sum: int64) m acc =
        if i >= nums.Length then
            acc
        else
            let sum = sum + nums.[i] - nums.[i - k]

            let m =
                match Map.tryFind nums.[i] m with
                | Some(v) -> Map.add nums.[i] (v + 1) m
                | None -> Map.add nums.[i] 1 m

            let count = Map.find nums.[i - k] m
            let m = if count = 1 then Map.remove nums.[i - k] m else m
            let acc = if Map.count m = k then max sum acc else acc
            maximumSubarraySum' (i + 1) nums sum m acc

    let nums = Array.map int64 nums

    let (m, sum) =
        nums
        |> Array.take k
        |> Array.fold
            (fun (acc, sum) n ->
                match Map.tryFind n acc with
                | Some(v) -> Map.add n (v + 1) acc, sum + n
                | None -> Map.add n 1 acc, sum + n)
            (Map.empty, 0L)

    let acc = if Map.count m = k then sum else 0
    maximumSubarraySum' k nums sum m acc

// 15
maximumSubarraySum [| 1; 5; 4; 2; 9; 9; 9 |] 3

// 0
maximumSubarraySum [| 4; 4; 4 |] 3
