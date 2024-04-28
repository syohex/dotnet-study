let addedInteger (nums1: int list) (nums2: int list) : int = (List.min nums2) - (List.min nums1)

// 3
addedInteger [ 2; 6; 4 ] [ 9; 7; 5 ]

// -5
addedInteger [ 10 ] [ 5 ]

// 0
addedInteger [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ]
