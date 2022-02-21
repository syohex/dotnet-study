let majorityElement1 (nums: int list) : int =
    nums
    |> List.countBy id
    |> List.sortBy (fun (_, count) -> count)
    |> List.rev
    |> List.head
    |> fst

let majorityElement (nums: int list) : int =
    let arr = nums |> List.sort |> List.toArray
    arr.[(Array.length arr) / 2]

// 3
majorityElement1 [ 3; 2; 3 ]
majorityElement [ 3; 2; 3 ]

// 2
majorityElement1 [ 2; 2; 1; 1; 1; 2; 2 ]
majorityElement [ 2; 2; 1; 1; 1; 2; 2 ]

// 1
majorityElement1 [ 1 ]
majorityElement [ 1 ]
