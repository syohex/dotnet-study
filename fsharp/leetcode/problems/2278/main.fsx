let percentageLetter (s: string) (letter: char) : int =
    let count =
        s
        |> Seq.fold (fun acc c -> if c = letter then acc + 1 else acc) 0

    ((double count) / (double s.Length)) * 100.0
    |> int

// 33
percentageLetter "foobar" 'o'

// 0
percentageLetter "jjjj" 'k'

// 66
percentageLetter "foooor" 'o'
