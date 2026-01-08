let maxDotProduct (nums1: int list) (nums2: int list) : int =
    let rec f i j nums1 nums2 cache : int * Map<(int * int), int> =
        match nums1, nums2 with
        | [], []
        | _, []
        | [], _ -> -1_000_000_00, cache
        | h1 :: t1, h2 :: t2 ->
            match Map.tryFind (i, j) cache with
            | Some v -> v, cache
            | None ->
                let v0 = h1 * h2
                let v1, cache = f (i + 1) (j + 1) t1 t2 cache
                let v2, cache = f (i + 1) j t1 nums2 cache
                let v3, cache = f i (j + 1) nums1 t2 cache
                let v = max v0 (max (v0 + v1) (max v2 v3))
                v, Map.add (i, j) v cache

    f 0 0 nums1 nums2 Map.empty |> fst

// 18
maxDotProduct [ 2; 1; -2; 5 ] [ 3; 0; -6 ]

// 21
maxDotProduct [ 3; -2 ] [ 2; -6; 7 ]

// -1
maxDotProduct [ -1; -1 ] [ 1; 1 ]

// -3
maxDotProduct [ -5; -1; -2 ] [ 3; 3; 5; 5 ]
