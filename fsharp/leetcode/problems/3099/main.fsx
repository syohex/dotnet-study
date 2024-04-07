let sumOfTheDigitsOfHarshadNumber (x: int) : int =
    let rec digitSum n sum =
        if n <= 0 then sum else digitSum (n / 10) (sum + n % 10)

    let sum = digitSum x 0
    if x % sum = 0 then sum else -1

// 9
sumOfTheDigitsOfHarshadNumber 18

// -1
sumOfTheDigitsOfHarshadNumber 23
