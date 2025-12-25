let maximumHappinessSum (happiness: int list) (k: int) : int64 =
    happiness
    |> List.sort
    |> List.rev
    |> List.indexed
    |> List.take k
    |> List.fold (fun acc (i, v) -> acc + int64 (max (v - i) 0)) 0L

// 4
maximumHappinessSum [ 1; 2; 3 ] 2

// 1
maximumHappinessSum [ 1; 1; 1; 1 ] 2

// 5
maximumHappinessSum [ 2; 3; 4; 5 ] 1
