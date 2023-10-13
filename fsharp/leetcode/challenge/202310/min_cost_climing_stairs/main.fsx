open System

let minCostClimingStairs (cost: int list) : int =
    let rec minCostClimingStairs' cost (prev2: int) prev1 =
        match cost with
        | [] -> Math.Min(prev2, prev1)
        | h :: t ->
            let v = Math.Min(h + prev1, h + prev2)
            minCostClimingStairs' t prev1 v

    match cost with
    | prev2 :: prev1 :: t -> minCostClimingStairs' t prev2 prev1
    | _ -> failwith "never reach here"

// 15
minCostClimingStairs [ 10; 15; 20 ]

// 6
minCostClimingStairs [ 1; 100; 1; 1; 1; 100; 1; 1; 100; 1 ]
