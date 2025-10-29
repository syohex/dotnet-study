let smallestNumber (n: int) : int =
    let rec smallestNumber' m =
        if m > n then m - 1 else smallestNumber' (m <<< 1)

    smallestNumber' 2

// 1
smallestNumber 1

// 7
smallestNumber 4

// 7
smallestNumber 5

// 15
smallestNumber 10

// 3
smallestNumber 3
