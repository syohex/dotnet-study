let mergeArrays (nums1: (int * int) list) (nums2: (int * int) list) : (int * int) list =
    let rec mergeArrays' nums1 nums2 acc =
        match nums1, nums2 with
        | [], [] -> List.rev acc
        | h :: t, [] -> mergeArrays' t nums2 (h :: acc)
        | [], h :: t -> mergeArrays' nums1 t (h :: acc)
        | (id1, v1) :: t1, (id2, v2) :: t2 ->
            if id1 = id2 then mergeArrays' t1 t2 ((id1, v1 + v2) :: acc)
            elif id1 < id2 then mergeArrays' t1 nums2 ((id1, v1) :: acc)
            else mergeArrays' nums1 t2 ((id2, v2) :: acc)

    mergeArrays' nums1 nums2 []

// [(1,6),(2,3),(3,2),(4,6)]
mergeArrays [ (1, 2); (2, 3); (4, 5) ] [ (1, 4); (3, 2); (4, 1) ]

// [[1,3],[2,4],[3,6],[4,3],[5,5]]
mergeArrays [ (2, 4); (3, 6); (5, 5) ] [ (1, 3); (4, 3) ]
