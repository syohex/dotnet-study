let threeConsecutiveOdds (arr: int list) : bool =
    arr
    |> List.windowed 3
    |> List.exists (fun v -> v |> List.forall (fun n -> n % 2 = 1))

// false
threeConsecutiveOdds [ 2; 6; 4; 1 ]

// true
threeConsecutiveOdds [ 1; 2; 34; 3; 4; 5; 7; 23; 12 ]
