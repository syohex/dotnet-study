let numRescueBoats (people: int list) (limit: int) : int =
    let rec numRescueBoats' (people: int []) limit left right count =
        if left > right then
            count
        else if people.[left] + people.[right] <= limit then
            numRescueBoats' people limit (left + 1) (right - 1) (count + 1)
        else
            numRescueBoats' people limit left (right - 1) (count + 1)

    let people' = people |> List.sort |> List.toArray
    numRescueBoats' people' limit 0 (people'.Length - 1) 0

// 1
numRescueBoats [ 1; 2 ] 3

// 3
numRescueBoats [ 3; 2; 2; 1 ] 3

// 4
numRescueBoats [ 3; 5; 3; 4 ] 5

// 1
numRescueBoats [ 1 ] 1
