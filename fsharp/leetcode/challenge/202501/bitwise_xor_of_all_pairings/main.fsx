let xorAllNums (nums1: int list) (nums2: int list) : int =
    let e1 = List.reduce (fun a b -> a ^^^ b) nums1
    let e2 = List.reduce (fun a b -> a ^^^ b) nums2

    match List.length nums1 % 2, List.length nums2 % 2 with
    | 0, 0 -> 0
    | 1, 0 -> e2
    | 0, 1 -> e1
    | 1, 1 -> e1 ^^^ e2
    | _ -> failwith "never reach here"

// 13
xorAllNums [ 2; 1; 3 ] [ 10; 2; 5; 0 ]

// 0
xorAllNums [ 1; 2 ] [ 3; 4 ]

// 9
xorAllNums [ 8; 6; 29; 2; 26; 16; 15; 29 ] [ 24; 12; 12 ]
