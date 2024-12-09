let isArraySpecial (nums: int list) (queries: (int * int) list) : bool list =
    let binarySearch (start, end_) (invalidIndexes: int[]) =
        let rec binarySearch' left right =
            if left > right then
                true
            else
                let mid = invalidIndexes.[left + (right - left) / 2]

                if mid < start then binarySearch' (mid + 1) right
                elif mid > end_ then binarySearch' left (mid - 1)
                else false

        binarySearch' 0 (invalidIndexes.Length - 1)

    let rec isArraySpecial' queries (invalidIndexes: int[]) acc =
        match queries with
        | [] -> List.rev acc
        | (s, e) :: t ->
            let ret = binarySearch (s + 1, e) invalidIndexes
            isArraySpecial' t invalidIndexes (ret :: acc)

    let invalidIndexes =
        nums
        |> List.windowed 2
        |> List.indexed
        |> List.filter (fun (_, v) ->
            let a, b = List.head v, List.item 1 v
            a % 2 = b % 2)
        |> List.map (fst >> (+) 1)
        |> List.toArray

    isArraySpecial' queries invalidIndexes []

// [false]
isArraySpecial [ 3; 4; 1; 2; 6 ] [ (0, 4) ]

// [false, true]
isArraySpecial [ 4; 3; 1; 6 ] [ (0, 2); (2, 3) ]
