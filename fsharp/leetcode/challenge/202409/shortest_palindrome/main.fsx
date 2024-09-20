let shortestPalindrome (s: string) : string =
    let rec findLongestPrefix (cs: char[]) left right =
        if right < 0 then
            left
        else if cs.[left] = cs.[right] then
            findLongestPrefix cs (left + 1) (right - 1)
        else
            findLongestPrefix cs left (right - 1)

    let rec shortestPalindrome' (cs: char[]) =
        if Array.length cs = 0 then
            [||]
        else
            let left = findLongestPrefix cs 0 (cs.Length - 1)

            if left = cs.Length then
                cs
            else
                let suffix = Array.skip left cs
                let revSuffix = Array.rev suffix
                Array.concat [ revSuffix; shortestPalindrome' (Array.take left cs); suffix ]

    shortestPalindrome' (s |> Seq.toArray) |> System.String.Concat

// "aaacecaaa"
shortestPalindrome "aacecaaa"

// "dcbabcd"
shortestPalindrome "abcd"
