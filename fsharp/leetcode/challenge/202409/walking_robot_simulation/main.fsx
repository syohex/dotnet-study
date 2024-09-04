let robotSim (commands: int list) (obstacles: (int * int) list) : int =
    let steps = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]
    let getKey x y = x * 60001 + y

    let rec robotSim' commands x y direction obstacles acc =
        match commands with
        | [] -> acc
        | h :: t ->
            match h with
            | -2 -> robotSim' t x y ((direction + 3) % 4) obstacles acc
            | -1 -> robotSim' t x y ((direction + 1) % 4) obstacles acc
            | n ->
                let x, y, acc, _ =
                    seq { 1..n }
                    |> Seq.fold
                        (fun (x, y, acc, ok) _ ->
                            if not ok then
                                x, y, acc, false
                            else
                                let x', y' = x + fst steps.[direction], y + snd steps.[direction]

                                if Set.contains (getKey x' y') obstacles then
                                    x, y, acc, false
                                else
                                    x', y', (max acc (x' * x' + y' * y')), ok)
                        (x, y, acc, true)

                robotSim' t x y direction obstacles acc

    let obstacles =
        obstacles |> List.fold (fun acc (x, y) -> Set.add (getKey x y) acc) Set.empty

    robotSim' commands 0 0 0 obstacles 0

// 25
robotSim [ 4; -1; 3 ] []

// 65
robotSim [ 4; -1; 4; -2; 4 ] [ (2, 4) ]

// 36
robotSim [ 6; -1; -1; 6 ] []
