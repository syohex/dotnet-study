let reconstructQueue (people: (int * int) list) : (int * int) list =
    let insert pos person queue =
        if pos = 0 then
            person :: queue
        else
            (List.take pos queue)
            @ (person :: (List.skip pos queue))


    let people' =
        people
        |> List.sortWith (fun (h1, t1) (h2, t2) ->
            if h1 = h2 then
                compare t1 t2
            else
                compare h2 h1)

    people'
    |> List.fold
        (fun acc person ->
            let (_, tallers) = person
            insert tallers person acc)
        []

// [(5,0),(7,0),(5,2),(6,1),(4,4),(7,1)]
reconstructQueue [ (7, 0)
                   (4, 4)
                   (7, 1)
                   (5, 0)
                   (6, 1)
                   (5, 2) ]

// [(4,0),(5,0),(2,2),(3,2),(1,4),(6,0)]
reconstructQueue [ (4, 0)
                   (5, 0)
                   (2, 2)
                   (3, 2)
                   (1, 4)
                   (6, 0) ]
