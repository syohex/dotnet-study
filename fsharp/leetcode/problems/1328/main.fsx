let breakPalindrome (palindrome: string) : string =
    let rec breakPalindrome' i limit (acc: char []) =
        if i >= limit then
            acc.[acc.Length - 1] <- 'b'
            acc |> System.String
        else if acc.[i] <> 'a' then
            acc.[i] <- 'a'
            acc |> System.String
        else
            breakPalindrome' (i + 1) limit acc

    if palindrome.Length = 1 then
        ""
    else
        let limit = palindrome.Length / 2
        breakPalindrome' 0 limit (palindrome |> Seq.toArray)

// "aaccba"
breakPalindrome "abccba"

// ""
breakPalindrome "a"

// "aabab"
breakPalindrome "aabaa"
