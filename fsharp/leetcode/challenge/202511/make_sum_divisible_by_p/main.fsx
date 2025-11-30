let minSubarray (nums: int list) (p: int) : int =
    let rec minSubarray' nums sum sumPos totalSum acc =
        match nums with
        | [] -> if acc = System.Int32.MaxValue then -1 else acc
        | (i, h) :: t ->
            let sum = (sum + h) % p
            let diff = (sum + p - totalSum) % p

            match Map.tryFind diff sumPos with
            | None -> minSubarray' t sum (Map.add sum i sumPos) totalSum acc
            | Some v ->
                let acc = min acc (i - v)
                minSubarray' t sum (Map.add sum i sumPos) totalSum acc

    let totalSum = nums |> List.reduce (fun a b -> (a + b) % p)
    if totalSum = 0 then
        0
    else
        let sumPos = Map.empty |> Map.add 0 -1
        minSubarray' (List.indexed nums) 0 sumPos totalSum System.Int32.MaxValue

// 1
minSubarray [3;1;4;2] 6

// 2
minSubarray [6;3;5;2] 9

// 0
minSubarray [1;2;3] 3
