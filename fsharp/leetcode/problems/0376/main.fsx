let wiggleMaxLength (nums: int list) : int =
    let rec wiggleMaxLength' pos nums prev up cache =
        match nums with
        | [] -> 0, cache
        | h :: t ->
            match Map.tryFind (pos, prev, up) cache with
            | Some (v) -> v, cache
            | None ->
                let diff = h - prev

                let ret1, cache' =
                    if (up && diff < 0) || (not up && diff > 0) then
                        let v1, cache' = wiggleMaxLength' (pos + 1) t h (not up) cache
                        v1 + 1, cache'
                    else
                        0, cache

                let ret2, cache'' = wiggleMaxLength' (pos + 1) t prev up cache'
                let ret = System.Math.Max(ret1, ret2)
                ret, Map.add (pos, prev, up) ret cache''

    let ret1, cache = wiggleMaxLength' 0 nums 1001 true Map.empty
    let ret2, _ = wiggleMaxLength' 0 nums -1001 false cache

    System.Math.Max(ret1, ret2)


// 6
wiggleMaxLength [ 1; 7; 4; 9; 2; 5 ]

// 7
wiggleMaxLength [ 1
                  17
                  5
                  10
                  13
                  15
                  10
                  5
                  16
                  8 ]

// 2
wiggleMaxLength [ 1
                  2
                  3
                  4
                  5
                  6
                  7
                  8
                  9 ]
