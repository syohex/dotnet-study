let isPathCrossing (path: string) : bool =
    let rec isPathCrossing' cs x y visited =
        match cs with
        | [] -> false
        | h :: t ->
            let x', y' =
                match h with
                | 'N' -> x, y + 1
                | 'E' -> x + 1, y
                | 'S' -> x, y - 1
                | 'W' -> x - 1, y
                | _ -> failwith "never reach here"

            if Set.contains (x', y') visited then
                true
            else
                isPathCrossing' t x' y' (Set.add (x', y') visited)

    isPathCrossing' (Seq.toList path) 0 0 (Set.empty |> Set.add (0, 0))

// false
isPathCrossing "NES"

// true
isPathCrossing "NESWW"
