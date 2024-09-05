let missingRolls (rolls: int list) (mean: int) (n: int) : int list =
    let rec missingRolls' n v m acc =
        if n = 0 then
            acc
        else if m > 0 then
            missingRolls' (n - 1) v (m - 1) ((v + 1) :: acc)
        else
            missingRolls' (n - 1) v 0 (v :: acc)

    let rollSum = List.sum rolls
    let sum = mean * (List.length rolls + n)
    let diff = sum - rollSum

    if diff < n || diff > 6 * n then
        []
    else
        let v = diff / n
        let m = diff % n
        missingRolls' n v m []

// [6, 6]
missingRolls [ 3; 2; 4; 3 ] 4 2

// [2,2,2,3]
missingRolls [ 1; 5; 6 ] 3 4

// []
missingRolls [ 1; 2; 3; 4 ] 6 4

// []
missingRolls [ 6; 3; 4; 3; 5; 3 ] 1 6
