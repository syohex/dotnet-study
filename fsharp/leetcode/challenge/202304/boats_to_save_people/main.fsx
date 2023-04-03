let numRescueBoats (people: int list) (limit: int) : int =
    let rec numRescueBoats' (people: int[]) left right limit acc =
        if left > right then
            acc
        else
            let acc' = acc + 1

            if people.[left] + people.[right] <= limit then
                numRescueBoats' people (left + 1) (right - 1) limit acc'
            else
                numRescueBoats' people left (right - 1) limit acc'

    let people' = people |> List.sort |> List.toArray
    numRescueBoats' people' 0 (people'.Length - 1) limit 0

// 1
numRescueBoats [ 1; 2 ] 3

// 3
numRescueBoats [ 3; 2; 2; 1 ] 3

// 4
numRescueBoats [ 3; 5; 3; 4 ] 5
