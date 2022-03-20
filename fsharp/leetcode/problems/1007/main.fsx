open System

let minDominoRotations (tops: int list) (bottoms: int list) : int =
    let rec minDominoRotations' value tops bottoms (tRotate: int) (bRotate: int) =
        match tops, bottoms with
        | [], [] -> Some(Math.Min(tRotate, bRotate))
        | _, []
        | [], _ -> failwith "never reach here"
        | x :: xs, y :: ys ->
            if x <> value && y <> value then
                None
            elif x <> value then
                minDominoRotations' value xs ys (tRotate + 1) bRotate
            else
                minDominoRotations' value xs ys tRotate (bRotate + 1)

    match minDominoRotations' tops.Head tops bottoms 0 0 with
    | Some (v) -> v
    | None ->
        match minDominoRotations' bottoms.Head bottoms tops 0 0 with
        | Some (v) -> v
        | None -> -1

// 2
minDominoRotations [ 2; 1; 2; 4; 2; 2 ] [
    5
    2
    6
    2
    3
    2
]

// -1
minDominoRotations [ 3; 5; 1; 2; 3 ] [
    3
    6
    3
    3
    4
]
