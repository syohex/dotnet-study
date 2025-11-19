let findFinalValue (nums: int list) (original: int) : int =
    let rec findFinalValue' nums original =
        if Set.contains original nums then
            findFinalValue' nums (original * 2)
        else
            original

    findFinalValue' (Set.ofList nums) original

// 24
findFinalValue [ 5; 3; 6; 1; 12 ] 3

// 4
findFinalValue [ 2; 7; 9 ] 4
