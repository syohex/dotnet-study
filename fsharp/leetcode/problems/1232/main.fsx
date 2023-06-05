let checkStraightLine (coordinates: (int * int) list) : bool =
    let diff (x1, y1) (x2, y2) = x2 - x1, y2 - y1

    let x1, y1 = List.head coordinates
    let x2, y2 = List.item 1 coordinates
    let xDiff, yDiff = diff (x1, y1) (x2, y2)

    if xDiff = 0 then
        List.forall (fun (x, _) -> x = x1) coordinates
    else
        coordinates
        |> List.windowed 2
        |> List.forall (fun lst ->
            match lst with
            | (a1, b1) :: (a2, b2) :: [] -> a2 - a1 = xDiff && b2 - b1 = yDiff
            | _ -> failwith "never reach here")


let coordinates1 = [ (1, 2); (2, 3); (3, 4); (4, 5); (5, 6); (6, 7) ]
// true
checkStraightLine coordinates1

let coordinates2 = [ (1, 1); (2, 2); (3, 4); (4, 5); (5, 6); (7, 7) ]
// false
checkStraightLine coordinates2

let coordinates3 = [ (0, 0); (0, 1); (0, -1) ]
// true
checkStraightLine coordinates3
