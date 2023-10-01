let reverseWord (s: string) : string =
    let ss = s.Split([| ' ' |]) |> Array.map (Seq.rev >> System.String.Concat)

    System.String.Join(' ', ss)

// "s'teL ekat edoCteeL tsetnoc"
reverseWord "Let's take LeetCode contest"

// "doG gniD"
reverseWord "God Ding"
