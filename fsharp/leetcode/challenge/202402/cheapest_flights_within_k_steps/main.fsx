let flightsToGraph flights =
    let rec flightsToGraph' flights acc =
        match flights with
        | [] -> acc
        | (src, dst, price) :: t ->
            match Map.tryFind src acc with
            | Some(v) -> flightsToGraph' t (Map.add src ((dst, price) :: v) acc)
            | None -> flightsToGraph' t (Map.add src [ (dst, price) ] acc)

    flightsToGraph' flights Map.empty

let findCheapestPrice (n: int) (flights: (int * int * int) list) (src: int) (dst: int) (k: int) : int =
    let rec findCheapestPrice' step q (minPrices: int[]) graph =
        match q with
        | [] ->
            if minPrices.[dst] = System.Int32.MaxValue then
                -1
            else
                minPrices.[dst]
        | _ ->
            if step > k then
                if minPrices.[dst] = System.Int32.MaxValue then
                    -1
                else
                    minPrices.[dst]
            else
                let q' = q |> List.filter (fun (city, price) -> price < minPrices.[city])
                q' |> List.iter (fun (city, price) -> minPrices.[city] <- price)

                let q'' =
                    q'
                    |> List.fold
                        (fun nextCities (city, total) ->
                            match Map.tryFind city graph with
                            | None -> nextCities
                            | Some(v) ->
                                v |> List.fold (fun acc (dst, price) -> (dst, total + price) :: acc) nextCities)
                        []

                findCheapestPrice' (step + 1) q'' minPrices graph

    let graph = flightsToGraph flights
    let minPrices = Array.init n (fun _ -> System.Int32.MaxValue)
    findCheapestPrice' -1 [ (src, 0) ] minPrices graph

// 700
findCheapestPrice 4 [ (0, 1, 100); (1, 2, 100); (2, 0, 100); (1, 3, 600); (2, 3, 200) ] 0 3 1

// 200
findCheapestPrice 3 [ (0, 1, 100); (1, 2, 100); (0, 2, 500) ] 0 2 1

// 500
findCheapestPrice 3 [ (0, 1, 100); (1, 2, 100); (0, 2, 500) ] 0 2 0
