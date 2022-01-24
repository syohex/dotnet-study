let arraySign (nums: int list) : int =
    if List.exists ((=) 0) nums then
        0
    else
        let negatives =
            nums |> List.filter ((>) 0) |> List.length

        if negatives % 2 = 0 then 1 else -1


// 1
arraySign [ -1; -2; -3; -4; 3; 2; 1 ]

// 0
arraySign [ 1; 5; 0; 2; -3 ]

// -1
arraySign [ -1; 1; -1; 1; -1 ]
