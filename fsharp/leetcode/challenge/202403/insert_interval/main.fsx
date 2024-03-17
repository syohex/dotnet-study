open System

let insert (intervals: (int * int) list) (newInterval: (int * int)) : (int * int) list =
    let rec insert' (intervals: (int * int) list) (start, endTime) merged ret =
        match intervals with
        | [] -> List.rev ret
        | (s, e) :: t ->
            if e < start || s > endTime then
                let ret' = if merged then ((start, endTime) :: ret) else ret
                insert' t (start, endTime) false ((s, e) :: ret')
            else
                let start' = Math.Min(start, s)
                let endTime' = Math.Max(endTime, e)
                insert' t (start', endTime') true ret

    insert' intervals newInterval false []

// [1,5],[6,9]]
insert [ (1, 3); (6, 9) ] (2, 5)

// [[1,2],[3,10],[12,16]]
insert [ (1, 2); (3, 5); (6, 7); (8, 10); (12, 16) ] (4, 8)
