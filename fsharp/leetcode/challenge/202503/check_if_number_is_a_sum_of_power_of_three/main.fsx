let checkPowersOfThree (n: int) : bool =
    let rec checkPowersOfThree' n v =
        if n = 0 then
            true
        elif n < 0 || v > n then
            false
        else
            checkPowersOfThree' (n - v) (v * 3) || checkPowersOfThree' n (v * 3)

    checkPowersOfThree' n 1

// true
checkPowersOfThree 12

// true
checkPowersOfThree 91

// false
checkPowersOfThree 21

// true
checkPowersOfThree 6378022
