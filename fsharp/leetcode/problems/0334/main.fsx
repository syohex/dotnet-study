let increasingTriplet (nums: int list) : bool =
    let rec increasingTriplet' nums first second =
        match nums with
        | [] -> false
        | h :: t ->
            if h <= first then
                increasingTriplet' t h second
            elif h <= second then
                increasingTriplet' t first h
            else
                true

    increasingTriplet' nums System.Int32.MaxValue System.Int32.MaxValue

// true
increasingTriplet [ 1; 2; 3; 4; 5 ]

// false
increasingTriplet [ 5; 4; 3; 2; 1 ]

// true
increasingTriplet [ 2; 1; 5; 0; 4; 6 ]

// false
increasingTriplet [ 1; 1; 1; 1; 1 ]

// false
increasingTriplet [ 1; 1; 2; 2; 2 ]
