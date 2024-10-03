let minSubarray (nums: int list) (p: int) : int =
    let len = List.length nums

    let rec sum nums p acc =
        match nums with
        | [] -> acc
        | h :: t -> sum t p ((acc + h) % p)

    let rec minSubarray' nums p total m acc ret =
        match nums with
        | [] -> if ret = len then -1 else ret
        | (i, h) :: t ->
            let acc' = (acc + h) % p
            let m' = Map.add acc' i m
            let diff = (acc' - total + p) % p

            match Map.tryFind diff m with
            | Some(v) -> minSubarray' t p total m' acc' (min ret (i - v))
            | None -> minSubarray' t p total m' acc' ret

    let total = sum nums p 0

    if total = 0 then
        0
    else
        let m = Map.empty |> Map.add 0 -1
        minSubarray' (List.indexed nums) p total m 0 len

// 1
minSubarray [ 3; 1; 4; 2 ] 6

// 2
minSubarray [ 6; 3; 5; 2 ] 9

// 0
minSubarray [ 1; 2; 3 ] 3
