let digits n = n |> string |> _.Length

let collectSequentialDigit digit =
    let rec createSequentialDigit n i digit acc =
        if i > digit then
            acc
        else
            createSequentialDigit (n + 1) (i + 1) digit (10 * acc + n)

    let rec collectSequentialDigit' n digit acc =
        if n > 10 - digit then
            List.rev acc
        else
            let seq = createSequentialDigit (n + 1) 2 digit n
            collectSequentialDigit' (n + 1) digit (seq :: acc)

    collectSequentialDigit' 1 digit []

let sequentialDigits (low: int) (high: int) : int list =
    let rec sequentialDigits' i limit low high acc =
        if i > limit then
            List.sort acc
        else
            let seqs = collectSequentialDigit i
            let matched = seqs |> List.filter (fun n -> n >= low && n <= high)
            sequentialDigits' (i + 1) limit low high (matched @ acc)
            
    let lowDigit, highDigit = digits low, digits high
    sequentialDigits' lowDigit highDigit low high []

// [123,234]
sequentialDigits 100 300

// [1234,2345,3456,4567,5678,6789,12345]
sequentialDigits 1000 13000
