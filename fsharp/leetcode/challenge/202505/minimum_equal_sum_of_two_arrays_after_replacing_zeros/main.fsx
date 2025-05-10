let minSum (nums1: int list) (nums2: int list) : int64 =
    let f nums =
        nums
        |> List.fold
            (fun (sums, zeros) n ->
                if n = 0 then
                    sums + 1L, zeros + 1
                else
                    sums + int64 n, zeros)
            (0L, 0)

    let sums1, zeros1 = f nums1
    let sums2, zeros2 = f nums2

    if (zeros1 = 0 && sums1 < sums2) || (zeros2 = 0 && sums2 < sums1) then
        -1
    else
        max sums1 sums2

// 12
minSum [ 3; 2; 0; 1; 0 ] [ 6; 5; 0 ]

// -1
minSum [ 2; 0; 2; 0 ] [ 1; 4 ]
