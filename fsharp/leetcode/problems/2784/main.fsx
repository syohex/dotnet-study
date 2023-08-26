let isGood (nums: int list) : bool =
    let max = List.max nums
    List.sort nums = (seq { 1..max } |> Seq.toList) @ [ max ]

// false
isGood [ 2; 1; 3 ]
// true
isGood [ 1; 3; 3; 2 ]
// true
isGood [ 1; 1 ]
// false
isGood [ 3; 4; 4; 1; 2; 1 ]
