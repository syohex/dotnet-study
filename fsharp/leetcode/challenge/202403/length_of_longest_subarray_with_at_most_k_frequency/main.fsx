let maxSubarrayLength (nums: int list) (k: int) : int =
    let rec moveLeft (nums: int[]) left right m =
        if left >= right || (Map.find nums.[right] m <= k) then
            left, m
        else
            let m' =
                match Map.tryFind nums.[left] m with
                | Some(v) -> Map.add nums.[left] (v - 1) m
                | None -> failwith "never reach here"

            moveLeft nums (left + 1) right m'

    let rec maxSubarrayLength' (nums: int[]) left right m ret =
        if right >= nums.Length then
            ret
        else
            let m' =
                match Map.tryFind nums.[right] m with
                | Some(v) -> Map.add nums.[right] (v + 1) m
                | None -> Map.add nums.[right] 1 m

            let left', m' = moveLeft nums left right m'
            let ret' = System.Math.Max(ret, right - left' + 1)
            maxSubarrayLength' nums left' (right + 1) m' ret'

    maxSubarrayLength' (List.toArray nums) 0 0 Map.empty 0

// 6
maxSubarrayLength [ 1; 2; 3; 1; 2; 3; 1; 2; 3 ] 2

// 2
maxSubarrayLength [ 1; 2; 1; 2; 1; 2; 1; 2 ] 1

//4
maxSubarrayLength [ 5; 5; 5; 5; 5; 5; 5; 5 ] 4
