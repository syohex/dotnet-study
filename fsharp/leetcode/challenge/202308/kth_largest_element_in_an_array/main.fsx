let findKthLargest (nums: int list) (k: int) : int =
    let rec findKthLargest' nums k =
        match nums with
        | [] -> failwith "never reach here"
        | h :: [] -> h
        | pivot :: _ ->
            let lefts, rights, mids =
                nums
                |> List.fold
                    (fun (lefts, rights, mids) n ->
                        if n > pivot then n :: lefts, rights, mids
                        elif n < pivot then lefts, n :: rights, mids
                        else lefts, rights, mids + 1)
                    ([], [], 0)

            let leftLength = List.length lefts
            let leftAndMid = leftLength + mids

            if k <= leftLength  then
                findKthLargest' lefts k
            elif leftAndMid < k then
                findKthLargest' rights (k - leftAndMid)
            else
                pivot

    findKthLargest' nums k

// 5
findKthLargest [ 3; 2; 1; 5; 6; 4 ] 2

// 4
findKthLargest [ 3; 2; 3; 1; 2; 4; 5; 5; 6 ] 4
