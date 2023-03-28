open System

let mincostTickets (days: int list) ((cost1, cost7, cost30): (int * int * int)) : int =
    let rec mincostTickets' day travelDays cache =
        if day > 365 then
            0, cache
        else
            match Map.tryFind day cache with
            | Some(v) -> v, cache
            | None ->
                if Set.contains day travelDays then
                    let ret1, cache1 = mincostTickets' (day + 1) travelDays cache
                    let ret7, cache7 = mincostTickets' (day + 7) travelDays cache1
                    let ret30, cache30 = mincostTickets' (day + 30) travelDays cache7

                    let ret = Math.Min(ret1 + cost1, Math.Min(ret7 + cost7, ret30 + cost30))
                    ret, Map.add day ret cache30
                else
                    mincostTickets' (day + 1) travelDays cache

    let travelDays = Set.ofList days
    mincostTickets' 1 travelDays Map.empty |> fst

// 11
mincostTickets [ 1; 4; 6; 7; 8; 20 ] (2, 7, 15)

// 17
mincostTickets [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 30; 31 ] (2, 7, 15)
