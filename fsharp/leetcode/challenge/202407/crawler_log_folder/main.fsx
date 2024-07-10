let minOperations (logs: string list) : int =
    let rec minOperations' logs depth =
        match logs with
        | [] -> depth
        | h :: t ->
            match h with
            | "../" -> minOperations' t (if depth > 0 then depth - 1 else 0)
            | "./" -> minOperations' t depth
            | _ -> minOperations' t (depth + 1)

    minOperations' logs 0

// 2
minOperations [ "d1/"; "d2/"; "../"; "d21/"; "./" ]

// 3
minOperations [ "d1/"; "d2/"; "./"; "d3/"; "../"; "d31/" ]

// 0
minOperations [ "d1/"; "../"; "../"; "../" ]
