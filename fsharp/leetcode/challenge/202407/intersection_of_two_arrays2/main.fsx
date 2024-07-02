let intersect (nums1: int list) (nums2: int list) : int list =
    let rec intersect' nums1 nums2 acc =
        match nums1, nums2 with
        | [], []
        | _, []
        | [], _ -> List.rev acc
        | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then intersect' t1 nums2 acc
            elif h1 > h2 then intersect' nums1 t2 acc
            else intersect' t1 t2 (h1 :: acc)

    intersect' (List.sort nums1) (List.sort nums2) []

// [2, 2]
intersect [ 1; 2; 2; 1 ] [ 2; 2 ]

// [4, 9]
intersect [ 4; 9; 5 ] [ 9; 4; 9; 8; 4 ]
