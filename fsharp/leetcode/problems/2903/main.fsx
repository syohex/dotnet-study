open System

let findIndices (nums: int list) (indexDifference: int) (valueDifference: int) : (int * int) =
    let rec findIndices' (nums: int list) i len =
        if len < indexDifference then
            -1, -1
        else
            match nums with
            | [] -> -1, -1
            | h :: t ->
                let ns = List.skip indexDifference nums

                match List.tryFindIndex (fun n -> Math.Abs(h - n) >= valueDifference) ns with
                | None -> findIndices' t (i + 1) (len - 1)
                | Some(j) -> i, j + indexDifference

    let len = List.length nums
    findIndices' nums 0 len

// (0, 3)
findIndices [ 5; 1; 4; 1 ] 2 4

// (0, 0)
findIndices [ 2; 1 ] 0 0

// (-1, -1)
findIndices [ 1; 2; 3 ] 2 4
