let minDays (bloomDay: int list) (m: int) (k: int) : int =
    let rec canMakeBouquets v bloomDay count acc =
        match bloomDay with
        | [] -> acc >= m
        | h :: t ->
            if h <= v then
                let count' = count + 1

                if count' = k then
                    canMakeBouquets v t 0 (acc + 1)
                else
                    canMakeBouquets v t count' acc
            else
                canMakeBouquets v t 0 acc

    let rec minDays' left right bloomDay ret =
        if left > right then
            if ret = System.Int32.MaxValue then -1 else ret
        else
            let mid = left + (right - left) / 2

            if canMakeBouquets mid bloomDay 0 0 then
                minDays' left (mid - 1) bloomDay mid
            else
                minDays' (mid + 1) right bloomDay ret

    let maxDays = List.max bloomDay
    minDays' 0 maxDays bloomDay (System.Int32.MaxValue)

// 3
minDays [ 1; 10; 3; 10; 2 ] 3 1

// -1
minDays [ 1; 10; 3; 10; 2 ] 3 2

// 12
minDays [ 7; 7; 7; 7; 12; 7; 7 ] 2 3
