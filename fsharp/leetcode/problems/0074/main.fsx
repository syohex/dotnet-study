let searchMatrix (matrix: int list list) (target: int) : bool =
    let rec toSingleDimentionArray matrix acc : int [] =
        match matrix with
        | [] -> acc |> List.rev |> List.toArray
        | h :: t ->
            let acc' = (h |> List.rev) @ acc
            toSingleDimentionArray t acc'

    let rec searchMatrix' (matrix: int []) left right target =
        if left > right then
            false
        else
            let mid = left + (right - left) / 2

            if matrix.[mid] = target then
                true
            elif target < matrix.[mid] then
                searchMatrix' matrix left (mid - 1) target
            else
                searchMatrix' matrix (mid + 1) right target

    let right = matrix.Length * matrix.Head.Length - 1
    let matrix' = toSingleDimentionArray matrix []
    searchMatrix' matrix' 0 right target

let matrix =
    [ [ 1; 3; 5; 7 ]
      [ 10; 11; 16; 20 ]
      [ 23; 30; 34; 60 ] ]

// true(all true)
seq {
    for i in [ 1; 7; 10; 16; 20; 23; 30; 34; 60 ] do
        searchMatrix matrix i
} |> Seq.forall ((=) true)


// true(all false)
seq {
    for i in [ 0; 8; 9; 21; 90 ] do
        searchMatrix matrix i
} |> Seq.forall ((=) false)
