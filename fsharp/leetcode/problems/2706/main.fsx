let buyChoco (prices: int list) (money: int) : int =
    let sum = prices |> List.sort |> List.take 2 |> List.sum
    if sum > money then money else money - sum

// 0
buyChoco [ 1; 2; 2 ] 3

// 3
buyChoco [ 2; 3; 3 ] 3
