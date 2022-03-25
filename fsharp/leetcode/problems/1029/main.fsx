let twoCitySchedCost (costs: (int * int) list) : int =
    let costs' =
        costs
        |> List.sortWith (fun (a0, a1) (b0, b1) -> compare (a0 - a1) (b0 - b1))

    let half = costs.Length / 2

    let costA =
        costs'
        |> List.take half
        |> List.map fst
        |> List.sum

    let costB =
        costs'
        |> List.skip half
        |> List.map snd
        |> List.sum

    costA + costB

// 110
twoCitySchedCost [ (10, 20)
                   (30, 200)
                   (400, 50)
                   (30, 20) ]

// 1859
twoCitySchedCost [ (259, 770)
                   (448, 54)
                   (926, 667)
                   (184, 139)
                   (840, 118)
                   (577, 469) ]

// 3086
twoCitySchedCost [ (515, 563)
                   (451, 713)
                   (537, 709)
                   (343, 819)
                   (855, 779)
                   (457, 60)
                   (650, 359)
                   (631, 42) ]
