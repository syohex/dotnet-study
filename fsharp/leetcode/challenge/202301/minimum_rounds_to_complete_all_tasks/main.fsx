let minimumRounds (tasks: int list) : int =
    let rec minimumRounds' values acc =
        match values with
        | [] -> acc
        | h :: t ->
            if h = 1 then
                -1
            else
                minimumRounds'
                    t
                    (acc
                     + (System.Math.Ceiling((double h) / 3.0) |> int))

    let freq =
        tasks
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | Some (v) -> Map.add n (v + 1) acc
                | None -> Map.add n 1 acc)
            Map.empty

    minimumRounds' (Map.values freq |> Seq.toList) 0

// 4
minimumRounds [ 2
                2
                3
                3
                2
                4
                4
                4
                4
                4 ]

// -1
minimumRounds [ 2; 3; 3 ]

// 20
minimumRounds [ 66
                66
                63
                61
                63
                63
                64
                66
                66
                65
                66
                65
                61
                67
                68
                66
                62
                67
                61
                64
                66
                60
                69
                66
                65
                68
                63
                60
                67
                62
                68
                60
                66
                64
                60
                60
                60
                62
                66
                64
                63
                65
                60
                69
                63
                68
                68
                69
                68
                61 ]
