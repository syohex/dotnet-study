open System

let findKDistantIndices (nums: int list) (key: int) (k: int) : int list =
    let keyIndices =
        nums
        |> List.mapi (fun i n -> (i, n))
        |> List.filter (fun (_, n) -> n = key)
        |> List.map fst

    nums
    |> List.mapi (fun i _ -> i)
    |> List.filter (fun i -> List.exists (fun index -> Math.Abs(i - index) <= k) keyIndices)

// [1;2;3;4;5;6]
findKDistantIndices [ 3; 4; 9; 1; 3; 9; 5 ] 9 1

// [0;1;2;3;4]
findKDistantIndices [ 2; 2; 2; 2; 2 ] 2 2
