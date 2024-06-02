let reverseString (s: char[]) =
    let rec reverString' (s:char[]) i =
        if i >= s.Length / 2 then
            s
        else
            let tmp = s.[i]
            s.[i] <- s.[s.Length - i - 1]
            s.[s.Length - i - 1] <- tmp
            reverString' s (i + 1)

    reverString' s 0

// ["o","l","l","e","h"]
reverseString ("hello" |> Seq.toArray)

// ["h","a","n","n","a","H"]
reverseString ("Hannah" |> Seq.toArray)
