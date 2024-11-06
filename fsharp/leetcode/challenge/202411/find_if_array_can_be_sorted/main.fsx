let canSortArray (nums: int list) : bool =
    let rec countBits n acc =
        if n = 0 then acc else countBits (n >>> 1) (acc + (n &&& 1))

    let sorted = List.sort nums
    // use Seq.sort to use stable sort
    let bitSorted =
        nums
        |> List.map (fun n -> n, countBits n 0)
        |> Seq.sortWith (fun (a, ones1) (b, ones2) -> if ones1 = ones2 then compare a b else 0)
        |> Seq.map fst
        |> Seq.toList

    sorted = bitSorted

// true
canSortArray [ 8; 4; 2; 30; 15 ]

// true
canSortArray [ 1; 2; 3; 4; 5 ]

// false
canSortArray [ 3; 16; 8; 4; 2 ]
