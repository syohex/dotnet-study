let findKthPositive (arr: int list) (k: int) =
    let rec findKthPositive' i missing s k =
        if Set.contains i s then
            findKthPositive' (i + 1) missing s k
        else
            let missing' = missing + 1

            if missing' = k then
                i
            else
                findKthPositive' (i + 1) missing' s k

    let s = Set.ofList arr
    findKthPositive' 1 0 s k

// 9
findKthPositive [ 2; 3; 4; 7; 11 ] 5

// 6
findKthPositive [ 1; 2; 3; 4 ] 2
