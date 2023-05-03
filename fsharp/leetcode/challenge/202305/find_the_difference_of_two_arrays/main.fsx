let findDifference (nums1: int list) (nums2: int list) : int list * int list =
    let s1 = Set.ofList nums1
    let s2 = Set.ofList nums2

    let ret1 = s1 |> Set.filter (fun n -> Set.contains n s2 |> not) |> Set.toList
    let ret2 = s2 |> Set.filter (fun n -> Set.contains n s1 |> not) |> Set.toList
    ret1, ret2

// [[1,3],[4,6]]
printfn "%A" (findDifference [ 1; 2; 3 ] [ 2; 4; 6 ])

// [[3],[]]
printfn "%A" (findDifference [ 1; 2; 3; 3 ] [ 1; 1; 2; 2 ])
