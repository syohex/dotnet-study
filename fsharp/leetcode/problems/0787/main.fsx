let flightsToGraph (flights: (int * int * int) list) : Map<int, (int * int) list> =
    let rec flightsToGraph' flights acc =
        match flights with
        | [] -> acc
        | (src, dst, cost) :: t ->
            let nexts =
                match Map.tryFind src acc with
                | Some(v) -> (dst, cost) :: v
                | None -> [ (dst, cost) ]

            flightsToGraph' t (Map.add src nexts acc)

    flightsToGraph' flights Map.empty

let findCheapestPrice (n: int) (flights: (int * int * int) list) (src: int) (dst: int) (k: int) : int =
    let rec findCheapestPrice' q stop graph (minCosts: int[]) =
        if stop >= k then
            if minCosts.[dst] = System.Int32.MaxValue then
                -1
            else
                minCosts.[dst]
        else
            let q' =
                q
                |> List.fold
                    (fun acc (city, costs) ->
                        match Map.tryFind city graph with
                        | None -> acc
                        | Some(v) ->
                            let nexts =
                                v
                                |> List.map (fun (next, cost) -> next, cost + costs)
                                |> List.filter (fun (next, totalCost) -> totalCost < minCosts.[next])

                            nexts @ acc)
                    []

            q' |> List.iter (fun (next, cost) -> minCosts.[next] <- cost)
            findCheapestPrice' q' (stop + 1) graph minCosts

    let graph = flightsToGraph flights
    let minCosts = Array.init n (fun _ -> System.Int32.MaxValue)
    findCheapestPrice' [ (src, 0) ] -1 graph minCosts

// 700
findCheapestPrice 4 [ (0, 1, 100); (1, 2, 100); (2, 0, 100); (1, 3, 600); (2, 3, 200) ] 0 3 1

// 200
findCheapestPrice 3 [ (0, 1, 100); (1, 2, 100); (1, 2, 100); (0, 2, 500) ] 0 2 1

// 500
findCheapestPrice 3 [ (0, 1, 100); (1, 2, 100); (1, 2, 100); (0, 2, 500) ] 0 2 0
