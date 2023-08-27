open System

let furtherestPointFromOrigin (moves: string) : int =
    let rec furtherestPointFromOrigin' cs (lefts: int) rights underscores =
        match cs with
        | [] -> Math.Abs(lefts - rights) + underscores
        | h :: t ->
            match h with
            | 'L' -> furtherestPointFromOrigin' t (lefts + 1) rights underscores
            | 'R' -> furtherestPointFromOrigin' t lefts (rights + 1) underscores
            | '_' -> furtherestPointFromOrigin' t lefts rights (underscores + 1)
            | _ -> failwith "never reach here"

    furtherestPointFromOrigin' (Seq.toList moves) 0 0 0

// 3
furtherestPointFromOrigin "L_RL__R"

// 5
furtherestPointFromOrigin "_R__LL_"

// 7
furtherestPointFromOrigin "_______"
