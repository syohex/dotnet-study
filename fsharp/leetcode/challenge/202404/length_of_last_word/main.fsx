let lengthOfLastWord (s: string) : int =
    s.TrimEnd()
    |> Seq.rev
    |> Seq.tryFindIndex ((=) ' ')
    |> Option.defaultValue s.Length

// 5
lengthOfLastWord "Hello World"

// 4
lengthOfLastWord "  fly me   to the moon"

// 6
lengthOfLastWord "luffy is still joyboy"

// 5
lengthOfLastWord "abcde"

// 5
lengthOfLastWord "    abcde     "
