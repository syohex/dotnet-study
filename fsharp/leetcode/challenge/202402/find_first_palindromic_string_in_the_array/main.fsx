let firstPalindrome (words: string list) : string =
    words
    |> List.tryFind (fun s -> Seq.toArray s = (Seq.toArray s |> Array.rev))
    |> Option.defaultValue ""

// "ada"
firstPalindrome [ "abc"; "car"; "ada"; "racecar"; "cool" ]

// "racecar"
firstPalindrome [ "notapalindrome"; "racecar" ]

// ""
firstPalindrome [ "def"; "ghi" ]
