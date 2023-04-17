let kidsWithCandies (candies: int list) (extraCandies: int) : bool list =
    let max = List.max candies
    candies |> List.map (fun n -> n + extraCandies >= max)

// [true, true, true, false, true]
kidsWithCandies [ 2; 3; 5; 1; 3 ] 3

// [true, false, false, false, false]
kidsWithCandies [ 4; 2; 1; 1; 2 ] 1

// [true, false, true]
kidsWithCandies [ 12; 1; 12 ] 10
