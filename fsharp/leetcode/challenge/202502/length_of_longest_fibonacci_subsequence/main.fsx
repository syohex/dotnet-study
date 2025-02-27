let lenLongestFibSubseq (arr: int list) : int =
    let rec countLongestFib prev1 prev2 s acc =
        if Set.contains (prev1 + prev2) s then
            countLongestFib (prev1 + prev2) prev1 s (acc + 1)
        else
            acc


    let rec lenLongestFibSubseq' arr s acc =
        match arr with
        | [] -> failwith "never reach here"
        | _ :: [] -> acc
        | h :: t ->
            let count =
                t
                |> List.fold
                    (fun acc n ->
                        if Set.contains (h + n) s then
                            max acc (countLongestFib (h + n) n s 3)
                        else
                            acc)
                    0

            lenLongestFibSubseq' t s (max acc count)

    let s = Set.ofList arr
    lenLongestFibSubseq' arr s 0

// 5
lenLongestFibSubseq [ 1; 2; 3; 4; 5; 6; 7; 8 ]

// 3
lenLongestFibSubseq [ 1; 3; 7; 11; 12; 14; 18 ]
