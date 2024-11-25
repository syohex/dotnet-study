let slidingPuzzle (board: int[,]) : int =
    let nexts = [| [ 1; 3 ]; [ 0; 2; 4 ]; [ 1; 5 ]; [ 0; 4 ]; [ 1; 3; 5 ]; [ 2; 4 ] |]
    let goal = [| 1; 2; 3; 4; 5; 0 |]

    let rec slidingPuzzle' q steps visited =
        match q with
        | [] -> -1
        | _ ->
            if List.exists ((=) goal) q then
                steps
            else
                let visited = q |> List.fold (fun acc v -> Set.add v acc) visited

                let q =
                    q
                    |> List.fold
                        (fun acc v ->
                            let zeroPos = Array.findIndex ((=) 0) v

                            nexts.[zeroPos]
                            |> List.fold
                                (fun acc i ->
                                    let next = Array.copy v
                                    let tmp = next.[i]
                                    next.[i] <- next.[zeroPos]
                                    next.[zeroPos] <- tmp
                                    next :: acc)
                                acc)
                        []
                    |> List.filter (fun v -> not <| Set.contains v visited)

                slidingPuzzle' q (steps + 1) visited

    let q = [ board |> Seq.cast<int> |> Seq.toArray ]
    slidingPuzzle' q 0 Set.empty

// 1
slidingPuzzle <| array2D [ [ 1; 2; 3 ]; [ 4; 0; 5 ] ]

// -1
slidingPuzzle <| array2D [ [ 1; 2; 3 ]; [ 5; 4; 0 ] ]

// 5
slidingPuzzle <| array2D [ [ 4; 1; 2 ]; [ 5; 0; 3 ] ]
