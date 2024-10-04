let dividePlayers (skill: int list) : int64 =
    let rec dividePlayers' skill target m (acc: int64) =
        match skill with
        | [] -> acc / 2L
        | h :: t ->
            let diff = target - h

            match Map.tryFind diff m with
            | Some(v) ->
                if v = 0 then
                    -1L
                else
                    let acc' = acc + int64 (h * diff)
                    dividePlayers' t target (Map.add diff (v - 1) m) acc'
            | None -> -1

    let sum = skill |> List.sum
    let target = sum / (List.length skill / 2)
    let m = skill |> List.countBy id |> Map.ofList
    dividePlayers' skill target m 0L

// 22
dividePlayers [ 3; 2; 5; 1; 3; 4 ]

// 12
dividePlayers [ 3; 4 ]

// -1
dividePlayers [ 1; 1; 2; 3 ]

// -1
dividePlayers [ 4; 4; 2; 3 ]
