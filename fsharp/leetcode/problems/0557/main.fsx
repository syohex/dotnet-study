let reverseString (s: string) : string =
    s.Split([| ' ' |])
    |> Array.map (fun s -> s.ToCharArray() |> Array.rev)
    |> Array.map System.String
    |> String.concat " "

// "s'teL ekat edoCteeL tsetnoc"
reverseString "Let's take LeetCode contest"

// "doG gniD"
reverseString "God Ding"
