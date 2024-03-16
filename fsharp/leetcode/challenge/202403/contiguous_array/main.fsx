open System

let findMaxLength (nums: int list) : int =
    let rec findMaxLength' i nums sum m ret =
        match nums with
        | [] -> ret
        | h :: t ->
            let sum' = if h = 1 then sum + 1 else sum - 1

            match Map.tryFind sum' m with
            | Some(v) -> findMaxLength' (i + 1) t sum' m (Math.Max(ret, i - v))
            | None -> findMaxLength' (i + 1) t sum' (Map.add sum' i m) ret

    findMaxLength' 0 nums 0 (Map.empty |> Map.add 0 -1) 0

// 2
findMaxLength [ 0; 1 ]

// 2
findMaxLength [ 0; 1; 0 ]
