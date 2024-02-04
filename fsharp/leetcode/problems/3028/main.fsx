let returnToTheBoundaryCount (nums: int list) : int =
    let rec returnToTheBoundaryCount' nums pos acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let pos' = pos + h

            if pos' = 0 then
                returnToTheBoundaryCount' t pos' (acc + 1)
            else
                returnToTheBoundaryCount' t pos' acc

    returnToTheBoundaryCount' nums 0 0

// 1
returnToTheBoundaryCount [ 2; 3; -5 ]

// 0
returnToTheBoundaryCount [ 3; 2; -3; -4 ]
