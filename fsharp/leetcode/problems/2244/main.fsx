let countProcesses (n: int) : int =
    let rec countProcesses' n acc =
        if n <= 2 then
            if n = 2 then acc + 1
            elif n = 1 then -1
            else acc
        else if n % 3 = 0 then
            acc + (n / 3)
        else
            countProcesses' (n - 2) (acc + 1)

    countProcesses' n 0

let minimumRounds (tasks: int list) : int =
    let rec minimumRounds' freqs acc =
        match freqs with
        | [] -> acc
        | (_, v) :: t ->
            let processes = countProcesses v

            if processes = -1 then
                -1
            else
                minimumRounds' t (acc + processes)

    let freqs =
        tasks
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | None -> Map.add n 1 acc
                | Some (v) -> Map.add n (v + 1) acc)
            Map.empty
        |> Map.toList

    minimumRounds' freqs 0

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
