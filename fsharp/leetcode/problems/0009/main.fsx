let isPalindrome (num: int) : bool =
    let rec numToDigits (num: int) (lst: int list): int list =
        if num = 0 then lst
        else numToDigits (num / 10) ((num % 10) :: lst)

    let rec isEqual (a: int list) (b: int list) : bool =
        if List.isEmpty a then true
        else if (List.head a) <> (List.head b) then false
        else isEqual (List.tail a) (List.tail b)

    if num < 0 then false
    else
        let orig = numToDigits num []
        let reversed = orig |> List.rev
        isEqual orig reversed

isPalindrome 121
isPalindrome -121
isPalindrome 10
