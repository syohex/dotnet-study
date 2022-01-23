let digits (n: int) : int =
    let rec digits' n len =
        if n = 0 then
            len
        else
            digits' (n / 10) (len + 1)

    if n = 0 then 1 else digits' n 0

let baseNumbers (digits: int) : (int * int) =
    let rec baseNumbers' n digit baseNum baseAdd =
        if n > digit then
            (baseNum, baseAdd)
        else
            baseNumbers' (n + 1) digit (baseNum * 10 + n) (baseAdd * 10 + 1)

    baseNumbers' 1 digits 0 0

let sequentialNumbers (digits: int) : int list =
    let rec sequentialNumbers' num baseAdd acc =
        if (num % 10) = 0 then
            acc |> List.rev
        else
            sequentialNumbers' (num + baseAdd) baseAdd (num :: acc)

    let (baseNum, baseAdd) = baseNumbers digits
    sequentialNumbers' baseNum baseAdd []

let sequentialDigits (low: int) (high: int) : int list =
    let rec sequentialDigits' currentDigit endDigits low high acc =
        if currentDigit > endDigits then
            acc
            |> List.filter (fun n -> n >= low && n <= high)
            |> List.sort
        else
            let nums = sequentialNumbers currentDigit
            sequentialDigits' (currentDigit + 1) endDigits low high (nums @ acc)

    let startDigits = digits low
    let endDigits = digits high
    sequentialDigits' startDigits endDigits low high []

sequentialDigits 100 300
sequentialDigits 1000 13000
