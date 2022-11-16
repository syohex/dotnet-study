let mutable picked = 0

let guess (n: int) : int =
    if n > picked then -1
    elif n = picked then 0
    else 1

let guessNumber (n: int) : int =
    let rec guessNumber' left right =
        let mid = left + ((right - left) / 2)

        match guess mid with
        | 0 -> mid
        | -1 -> guessNumber' left (mid - 1)
        | 1 -> guessNumber' (mid + 1) right
        | _ -> failwith "never reach here"

    guessNumber' 1 n

picked <- 6
// 6
guessNumber 10

picked <- 1
// 1
guessNumber 1

picked <- 1
// 1
guessNumber 2
