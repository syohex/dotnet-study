let intersection (nums1: int list) (nums2: int list) : int list =
    (Set.ofList nums1, Set.ofList nums2) ||> Set.intersect |> Set.toList

// [2]
intersection [ 1; 2; 2; 1 ] [ 2; 2 ]

// [9,4]
intersection [ 4; 9; 5 ] [ 9; 4; 9; 8; 4 ]
