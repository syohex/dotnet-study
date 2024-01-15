let winOrLose matches =
    let rec winOrLose' matches winner loser player =
        match matches with
        | [] -> winner, loser, Set.toList player |> List.sort
        | (win, lose) :: t ->
            let winner' =
                match Map.tryFind win winner with
                | Some(v) -> Map.add win (v + 1) winner
                | None -> Map.add win 1 winner

            let loser' =
                match Map.tryFind lose loser with
                | Some(v) -> Map.add lose (v + 1) loser
                | None -> Map.add lose 1 loser

            winOrLose' t winner' loser' (player |> Set.add win |> Set.add lose)

    winOrLose' matches Map.empty Map.empty Set.empty


let findWinners (matches: (int * int) list) : (int list * int list) =
    let rec findWinners' player winner loser notLost oneLose =
        match player with
        | [] -> List.rev notLost, List.rev oneLose
        | h :: t ->
            if Map.containsKey h winner && not <| Map.containsKey h loser then
                findWinners' t winner loser (h :: notLost) oneLose
            else
                let loses = Map.tryFind h loser |> Option.defaultValue 0

                if loses = 1 then
                    findWinners' t winner loser notLost (h :: oneLose)
                else
                    findWinners' t winner loser notLost oneLose

    let winner, loser, player = winOrLose matches
    findWinners' player winner loser [] []

// [[1,2,10],[4,5,7,8]]
findWinners
    [ (1, 3)
      (2, 3)
      (3, 6)
      (5, 6)
      (5, 7)
      (4, 5)
      (4, 8)
      (4, 9)
      (10, 4)
      (10, 9) ]

// [[1,2,5,6],[]]
findWinners [ (2, 3); (1, 3); (5, 4); (6, 4) ]
