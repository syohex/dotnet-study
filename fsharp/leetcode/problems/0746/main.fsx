let minCostClimbingStairs (cost: int list) : int =
    let rec minCostClimbingStairs' cost (prev2: int) prev1 =
        match cost with
        | [] -> System.Math.Min(prev2, prev1)
        | h :: t ->
            let val2 = h + prev2
            let val1 = h + prev1

            minCostClimbingStairs' t prev1 (System.Math.Min(val2, val1))

    match cost with
    | prev2 :: prev1 :: rest -> minCostClimbingStairs' rest prev2 prev1
    | _ -> failwith "never reach here"

// 15
minCostClimbingStairs [ 10; 15; 20 ]

// 6
minCostClimbingStairs [ 1
                        100
                        1
                        1
                        1
                        100
                        1
                        1
                        100
                        1 ]
