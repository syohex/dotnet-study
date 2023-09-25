let findTheDifference (s: string) (t: string) : char =
    let sumChars = Seq.map int >> Seq.sum
    (sumChars t) - (sumChars s) |> char

// e
findTheDifference "abcd" "abcde"

// y
findTheDifference "" "y"
