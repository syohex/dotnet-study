let canVisitAllRooms (rooms: int list[]) : bool =
    let rec canVisitAllRooms' room (rooms: int list[]) visited =
        if rooms.Length = (Seq.length visited) then
            true, visited
        else
            rooms.[room]
            |> List.fold
                (fun (ok, visited) nextRoom ->
                    if ok then
                        ok, visited
                    elif Set.contains nextRoom visited then
                        false, visited
                    else
                        canVisitAllRooms' nextRoom rooms (Set.add nextRoom visited))
                (false, visited)

    canVisitAllRooms' 0 rooms (Set.add 0 Set.empty) |> fst

// true
canVisitAllRooms [| [ 1 ]; [ 2 ]; [ 3 ]; [] |]

// false
canVisitAllRooms [| [ 1; 3 ]; [ 3; 0; 1 ]; [ 2 ]; [ 0 ] |]

// true
canVisitAllRooms [| [ 2; 3 ]; []; [ 2 ]; [ 1; 3 ] |]
