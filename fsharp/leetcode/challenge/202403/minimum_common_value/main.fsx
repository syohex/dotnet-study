let rec getCommon (nums1: int list) (nums2: int list) : int =
    match nums1, nums2 with
    | [], []
    | _, []
    | [], _ -> -1
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then h1
        elif h1 > h2 then getCommon nums1 t2
        else getCommon t1 nums2

// 2
getCommon [ 1; 2; 3 ] [ 2; 4 ]

// 2
getCommon [ 1; 2; 3; 6 ] [ 2; 3; 4; 5 ]

// -1
getCommon [ 1; 2; 3 ] [ 4; 5; 6 ]

// 5
getCommon [ 1; 2; 5 ] [ 5 ]
