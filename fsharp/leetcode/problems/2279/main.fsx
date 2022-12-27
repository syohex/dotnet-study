let maximumBags (capacity: int list) (rocks: int list) (additionalRocks: int) : int =
    let rec maximumBags' diffs additionalRocks acc =
        match diffs with
        | [] -> acc
        | h :: t ->
            if h <= additionalRocks then
                maximumBags' t (additionalRocks - h) (acc + 1)
            else
                acc

    let diffs = List.zip capacity rocks |> List.map (fun (a, b) -> a - b)
    maximumBags' diffs additionalRocks 0

// 3
maximumBags [ 2; 3; 4; 5 ] [ 1; 2; 4; 4 ] 2

// 3
maximumBags [ 10; 2; 2 ] [ 2; 2; 0 ] 100
