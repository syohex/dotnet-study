open System

let longestSubsequence (arr: int list) (difference: int) : int =
    arr
    |> List.fold
        (fun (m, ret) n ->
            match Map.tryFind (n - difference) m with
            | Some(v) -> Map.add n (v + 1) m, Math.Max(ret, v + 1)
            | None -> Map.add n 1 m, ret)
        (Map.empty, 1)
    |> snd

// 4
longestSubsequence [ 1; 2; 3; 4 ] 1

// 1
longestSubsequence [ 1; 3; 5; 7 ] 1

// 4
longestSubsequence [ 1; 5; 7; 8; 5; 3; 4; 2; 1 ] -2
