let rec toDigits n acc =
    if n = 0
    then
        acc
    else
        toDigits (n / 10) ((n % 10) :: acc)

let rec addDigits (n: int) : int =
    let sum = (toDigits n [])|> List.sum
    if sum < 10
    then    
        sum
    else
        addDigits sum

// 2
addDigits 38

// 0
addDigits 0