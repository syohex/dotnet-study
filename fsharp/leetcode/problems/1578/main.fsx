open System

let minCost (color: string) (neededTime: int list) : int =
    let rec calculateCost color cs neededTime total (max: int) =
        match cs, neededTime with
        | [], [] -> total - max, [], []
        | [], _
        | _, [] -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            if color = h1 then
                calculateCost color t1 t2 (total + h2) (Math.Max(max, h2))
            else
                total - max, t1, t2

    let rec minCost' cs neededTime acc =
        match cs, neededTime with
        | [], [] -> acc
        | [], _
        | _, [] -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            let cost, cs', neededTime' = calculateCost h1 t1 t2 h2 h2
            minCost' cs' neededTime' (acc + cost)

    minCost' (Seq.toList color) neededTime 0

// 3
minCost "abaac" [ 1; 2; 3; 4; 5 ]

// 0
minCost "abc" [ 1; 2; 3 ]

// 2
minCost "aabaa" [ 1; 2; 3; 4; 1 ]
