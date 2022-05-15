open System

let largestCombination (candidates: int list) : int =
    let rec largestCombination' num limit cands ret =
        if num > limit then
            ret
        else
            let sum =
                cands
                |> List.fold
                    (fun acc n ->
                        if (n &&& num) <> 0 then
                            acc + 1
                        else
                            acc)
                    0

            largestCombination' (num <<< 1) limit cands (Math.Max(ret, sum))

    let limit = candidates |> List.max
    largestCombination' 1 limit candidates 0

// 4
largestCombination [ 16
                     17
                     71
                     62
                     24
                     14 ]

// 2
largestCombination [ 8; 8 ]
