let isAcronym (words: string list) (s: string) : bool =
    let t = words |> List.fold (fun acc word -> (Seq.head word) :: acc) [] |> List.rev
    t = (Seq.toList s)

// true
isAcronym [ "alice"; "bob"; "charlie" ] "abc"

// false
isAcronym [ "an"; "apple" ] "a"

// true
isAcronym [ "never"; "gonna"; "give"; "up"; "on"; "you" ] "ngguoy"
