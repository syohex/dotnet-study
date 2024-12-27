let maxScoreSightseeingPair (values: int list) : int =
    let rec maxScoreSightseeingPair' values maxVal acc =
        match values with
        | [] -> acc
        | (i, n) :: t ->
            let sum = maxVal + n - i
            maxScoreSightseeingPair' t (max maxVal (n + i)) (max acc sum)

    match values |> List.indexed with
    | [] -> failwith "never reach here"
    | (_, h) :: t -> maxScoreSightseeingPair' t h 0

// 11
maxScoreSightseeingPair [ 8; 1; 5; 2; 6 ]

// 2
maxScoreSightseeingPair [ 1; 2 ]
