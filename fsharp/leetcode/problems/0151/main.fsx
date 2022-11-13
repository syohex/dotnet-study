let reverseWords (s: string) : string =
    s.Split [| ' ' |]
    |> Seq.filter (System.String.IsNullOrEmpty >> not)
    |> Seq.rev
    |> Seq.toArray
    |> String.concat " "

// "blue is sky the"
reverseWords "the sky is blue"

// "world hello"
reverseWords "    hello   world"

// "example good a"
reverseWords "a good    example"
