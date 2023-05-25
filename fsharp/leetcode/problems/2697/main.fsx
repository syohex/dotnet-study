let makeSmallestPalindrome (s: string) : string =
    let rec makeSmallestPalindrome' (s: string) left right (acc: char array) =
        if left > right then
            System.String.Concat acc
        else
            if s.[left] < s.[right] then
                acc.[left] <- s.[left]
                acc.[right] <- s.[left]
            else
                acc.[left] <- s.[right]
                acc.[right] <- s.[right]

            makeSmallestPalindrome' s (left + 1) (right - 1) acc

    let len = s.Length
    let acc = Array.zeroCreate len
    makeSmallestPalindrome' s 0 (len - 1) acc

// efcfe
makeSmallestPalindrome "egcfe"

// abba
makeSmallestPalindrome "abcd"

// neven
makeSmallestPalindrome "neven"
