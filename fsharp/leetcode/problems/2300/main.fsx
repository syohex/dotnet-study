let successfulPairs (spells: int list) (potions: int list) (success: int) : int list =
    let rec lowerBound spell (potions: int []) left right success =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2
            let v = spell * potions.[mid]

            if v < success then
                lowerBound spell potions (mid + 1) right success
            else
                lowerBound spell potions left (mid - 1) success

    let rec successfulPairs' spells (potions: int []) success acc =
        match spells with
        | [] -> List.rev acc
        | h :: t ->
            let right = potions.Length - 1
            let pos = lowerBound h potions 0 right success
            let acc' = (potions.Length - pos) :: acc
            successfulPairs' t potions success acc'

    let potions' = List.sort potions |> List.toArray
    successfulPairs' spells potions' success []

// [4, 0, 3]
successfulPairs [ 5; 1; 3 ] [ 1; 2; 3; 4; 5 ] 7

// [2, 0, 2]
successfulPairs [ 3; 1; 2 ] [ 8; 5; 8 ] 16
