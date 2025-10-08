let successfulPairs (spells: int list) (potions: int list) (success: int64) : int list =
    let rec lowerBound left right fn (vals: int[]) =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if fn vals.[mid] < success then
                lowerBound (mid + 1) right fn vals
            else
                lowerBound left mid fn vals

    let potions = potions |> List.sort |> List.toArray
    let len = potions.Length

    spells
    |> List.map (fun spell ->
        let pos = lowerBound 0 len (fun v -> int64 <| v * spell) potions
        len - pos)

// [4,0,3]
successfulPairs [ 5; 1; 3 ] [ 1; 2; 3; 4; 5 ] 7L

// [2,0,2]
successfulPairs [ 3; 1; 2 ] [ 8; 5; 8 ] 16L
