let totalFruits (fruits: int list) : int =
    let rec adjustWindow (fruits: int[]) left m =
        if (Map.count) m <= 2 then
            left, m
        else
            let v = Map.find fruits.[left] m

            if v = 1 then
                adjustWindow fruits (left + 1) (Map.remove fruits.[left] m)
            else
                adjustWindow fruits (left + 1) (Map.add fruits.[left] (v - 1) m)

    let rec totalFruits' (fruits: int[]) left right m acc =
        if right >= fruits.Length then
            acc
        else
            let n = Map.tryFind fruits.[right] m |> Option.defaultValue 0
            let m = Map.add fruits.[right] (n + 1) m
            let left, m = adjustWindow fruits left m
            totalFruits' fruits left (right + 1) m (max acc (right - left + 1))

    totalFruits' (List.toArray fruits) 0 0 Map.empty 0

// 3
totalFruits [ 1; 2; 1 ]

// 3
totalFruits [ 0; 1; 2; 2 ]

// 4
totalFruits [ 1; 2; 3; 2; 2 ]

// 5
totalFruits [ 3; 3; 3; 1; 2; 1; 1; 2; 3; 3; 4 ]
