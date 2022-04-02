let rec isPalindrome (cs: char []) (left: int) (right: int) : bool =
    if left >= right then
        true
    else if cs.[left] <> cs.[right] then
        false
    else
        isPalindrome cs (left + 1) (right - 1)

let validPalindrome (s: string) : bool =
    let rec validPalindrome' (cs: char []) left right =
        if left >= right then
            true
        else if cs.[left] <> cs.[right] then
            isPalindrome cs (left + 1) right
            || isPalindrome cs left (right - 1)
        else
            validPalindrome' cs (left + 1) (right - 1)

    let cs = s |> Seq.toArray
    validPalindrome' cs 0 (cs.Length - 1)

// true
validPalindrome "aba"

// true
validPalindrome "abca"

// false
validPalindrome "abc"

// true
validPalindrome "deeee"
