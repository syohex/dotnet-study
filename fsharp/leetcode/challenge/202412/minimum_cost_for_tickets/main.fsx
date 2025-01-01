let mincostTickets (days: int list) (costs: int[]) : int =
    let rec minCostTickets' days cache =
        match days with
        | [] -> 0, cache
        | (i, h) :: t ->
            match Map.tryFind i cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache = minCostTickets' t cache
                let ret2, cache = minCostTickets' (List.skipWhile (fun (_, n) -> n - h < 7) t) cache

                let ret3, cache =
                    minCostTickets' (List.skipWhile (fun (_, n) -> n - h < 30) t) cache

                let ret = min (ret1 + costs.[0]) (min (ret2 + costs.[1]) (ret3 + costs.[2]))
                ret, Map.add i ret cache

    minCostTickets' (List.indexed days) Map.empty |> fst

// 11
mincostTickets [ 1; 4; 6; 7; 8; 20 ] [| 2; 7; 15 |]
// 17
mincostTickets [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 30; 31 ] [| 2; 7; 15 |]
