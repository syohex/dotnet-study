let largestDigit (num: int) : int =
    let rec largetDigit' num acc =
        if num = 0 then
            acc
        else
            largetDigit' (num / 10) (System.Math.Max(acc, num % 10))

    largetDigit' num 0

let encrypt (num: int) : int =
    let rec encrypt' num largest acc =
        if num = 0 then
            acc
        else
            encrypt' (num / 10) largest (acc * 10 + largest)

    encrypt' num (largestDigit num) 0

let sumOfEncryptedInt (nums: int list) : int = nums |> List.map encrypt |> List.sum

// 6
sumOfEncryptedInt [ 1; 2; 3 ]

// 66
sumOfEncryptedInt [ 10; 21; 32 ]
