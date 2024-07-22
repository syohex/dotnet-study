let sortPeople (names: string list) (heights: int list) : string list =
    List.zip names heights
    |> List.sortWith (fun (_, h1) (_, h2) -> compare h2 h1)
    |> List.map fst

// ["Mary","Emma","John"]
sortPeople [ "Mary"; "John"; "Emma" ] [ 180; 165; 170 ]

// ["Bob","Alice","Bob"]
sortPeople [ "Alice"; "Bob"; "Bob" ] [ 155; 185; 150 ]
