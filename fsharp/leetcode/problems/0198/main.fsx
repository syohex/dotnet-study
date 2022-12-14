let rob (nums: int list) : int =
    let rec rob' (nums: int list) prev2 prev1 =
        match nums with
        | [] -> prev1
        | h :: t ->
            let prev1' = System.Math.Max(prev1, prev2 + h)
            rob' t prev1 prev1'

    rob' nums 0 0

// 4
rob [ 1; 2; 3; 1 ]

// 12
rob [ 2; 7; 9; 3; 1 ]
