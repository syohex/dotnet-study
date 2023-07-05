open System

let longestSubarray (nums: int list) : int =
    let rec collectOnes nums i start acc =
        match nums with
        | [] ->
            match start with
            | Some(v) -> ((v, i - v) :: acc) |> List.rev
            | None -> acc |> List.rev
        | h :: t ->
            match start with
            | Some(v) ->
                if h = 1 then
                    collectOnes t (i + 1) start acc
                else
                    collectOnes t (i + 1) None ((v, i - v) :: acc)
            | None ->
                if h = 1 then
                    collectOnes t (i + 1) (Some(i)) acc
                else
                    collectOnes t (i + 1) None acc

    let rec longestSubarray' pairs (prevStart, prevLength) (ret: int) =
        match pairs with
        | [] -> ret
        | (start, length) :: t ->
            if prevStart + prevLength + 1 = start then
                longestSubarray' t (start, length) (Math.Max(ret, length + prevLength))
            else
                longestSubarray' t (start, length) (Math.Max(ret, length))

    let len = List.length nums
    let pairs = collectOnes nums 0 None []

    match pairs with
    | [] -> 0
    | (_, length) :: [] -> if len = length then len - 1 else length
    | prev :: t -> longestSubarray' t prev 0

// 3
longestSubarray [ 1; 1; 0; 1 ]

// 5
longestSubarray [ 0; 1; 1; 1; 0; 1; 1; 0; 1 ]

// 2
longestSubarray [ 1; 1; 1 ]

// 2
longestSubarray [ 1; 0; 1; 0; 1; 0 ]

// 1
longestSubarray [ 1; 0; 0; 0; 0 ]
