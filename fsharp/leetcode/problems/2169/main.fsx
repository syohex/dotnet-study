let countOperations (num1: int) (num2: int) : int =
    let rec countOperations' num1 num2 count =
        if num1 = 0 || num2 = 0 then
            count
        else if num1 > num2 then
            countOperations' (num1 - num2) num2 (count + 1)
        else
            countOperations' num1 (num2 - num1) (count + 1)

    countOperations' num1 num2 0

// 3
countOperations 2 3

// 1
countOperations 10 10

// 0
countOperations 0 0
