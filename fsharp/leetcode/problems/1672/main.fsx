let maximumWealth (accounts: int list list) : int =
    accounts |> List.map List.sum |> List.max

// 6
maximumWealth [ [ 1 .. 3 ]
                [ 3; 2; 1 ] ]

// 10
maximumWealth [ [ 1; 5 ]
                [ 7; 3 ]
                [ 3; 5 ] ]

// 17
maximumWealth [ [ 2; 8; 7 ]
                [ 7; 1; 3 ]
                [ 1; 9; 5 ] ]
