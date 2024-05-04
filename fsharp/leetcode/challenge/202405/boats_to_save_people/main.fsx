let numRescueBoats (people: int list) (limit: int) : int =
    let rec numRescueBoats' (people: int[]) left right ret =
        if left > right then
            ret
        else
            let ret' = ret + 1

            if people.[left] + people.[right] <= limit then
                numRescueBoats' people (left + 1) (right - 1) ret'
            else
                numRescueBoats' people left (right - 1) ret'

    let people = people |> List.sort |> List.toArray
    numRescueBoats' people 0 (people.Length - 1) 0

// 1
numRescueBoats [ 1; 2 ] 3

// 3
numRescueBoats [ 3; 2; 2; 1 ] 3

// 4
numRescueBoats [ 3; 5; 3; 4 ] 5
