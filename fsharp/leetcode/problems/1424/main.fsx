let findDiagonalOrder (nums: int array array) : int list =
    let rows = Array.length nums.[0]

    let toDiagnalDict (nums: int array array) =
        let mutable m = Map.empty

        for i in 0 .. (rows - 1) do
            for j in 0 .. (Array.length nums.[i] - 1) do
                let index = i + j

                match Map.tryFind index m with
                | Some(v) -> m <- Map.add index (nums.[i].[j] :: v) m
                | None -> m <- Map.add index [ nums.[i].[j] ] m

        m

    let rec findDiagonalOrder' i m acc =
        match Map.tryFind i m with
        | None -> acc
        | Some(v) -> findDiagonalOrder' (i + 1) m (acc @ v)

    let m = toDiagnalDict nums
    findDiagonalOrder' 0 m []

let nums1 = [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; 9 |] |]
// [1,4,2,7,5,3,8,6,9]
findDiagonalOrder nums1

let nums2 =
    [| [| 1; 2; 3; 4; 5 |]
       [| 6; 7 |]
       [| 8 |]
       [| 9; 10; 11 |]
       [| 12; 13; 14; 15; 16 |] |]
// [1,6,2,8,7,3,9,4,12,10,5,13,11,14,15,16]
findDiagonalOrder nums2
