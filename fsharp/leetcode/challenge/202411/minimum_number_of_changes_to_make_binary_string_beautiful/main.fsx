let minChanges (s: string) : int =
    let rec minChanges' cs prev count acc =
        match cs with
        | [] -> acc
        | h :: t ->
            if h = prev then minChanges' t prev (count + 1) acc
            else if count % 2 = 0 then minChanges' t h 1 acc
            else minChanges' t h 2 (acc + 1)

    match Seq.toList s with
    | [] -> 0
    | h :: t -> minChanges' t h 1 0

// 2
minChanges "1001"

// 1
minChanges "10"

// 0
minChanges "0000"
