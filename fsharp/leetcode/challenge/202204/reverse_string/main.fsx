let reverseString (s: char []) =
    let rec reverseString' (s: char []) left right =
        if left >= right then
            ()
        else
            let tmp = s.[left]
            s.[left] <- s.[right]
            s.[right] <- tmp

            reverseString' s (left + 1) (right - 1)

    reverseString' s 0 (s.Length - 1)
    ()


let s1 = [| 'h'; 'e'; 'l'; 'l'; 'o' |]
reverseString s1
// [|'o', 'l', 'l', 'e', 'h'|]
s1

let s2 = [| 'H'; 'a'; 'n'; 'n'; 'a'; 'h' |]
reverseString s2
// [|'h', 'a', 'n', 'n', 'a', 'H'|]
s2
