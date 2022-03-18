let maxScoreIndices (nums: int list) : int list =
    let rec maxScoreIndices' nums left right max acc =
        match nums with
        | [] -> acc |> List.rev
        | (i, h) :: t ->
            let (left, right) =
                if h = 0 then
                    left + 1, right
                else
                    left, right - 1

            let score = left + right

            if score > max then
                maxScoreIndices' t left right score [ i ]
            elif score = max then
                maxScoreIndices' t left right score (i :: acc)
            else
                maxScoreIndices' t left right max acc

    let right = nums |> List.reduce (fun a b -> a + b)
    let numWithIndex = nums |> List.mapi (fun i n -> i + 1, n)
    maxScoreIndices' numWithIndex 0 right right [ 0 ]

// [2,4]
maxScoreIndices [ 0; 0; 1; 0 ]

// [3]
maxScoreIndices [ 0; 0; 0 ]

// [0]
maxScoreIndices [ 1; 1 ]
