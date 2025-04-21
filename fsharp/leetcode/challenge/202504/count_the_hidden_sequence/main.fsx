let numberOfArrays (differences: int list) (lower: int) (upper: int) : int =
    let rec numberOfArrays' differences sum minVal maxVal =
        match differences with
        | [] -> upper - lower + 1 - (maxVal - minVal)
        | h :: t ->
            let sum = sum + h
            let minVal = min minVal sum
            let maxVal = max maxVal sum

            if maxVal - minVal > upper - lower then
                0
            else
                numberOfArrays' t sum minVal maxVal

    numberOfArrays' differences 0 0 0

// 2
numberOfArrays [ 1; -3; 4 ] 1 6

// 4
numberOfArrays [ 3; -4; 5; 1; -2 ] -4 5
