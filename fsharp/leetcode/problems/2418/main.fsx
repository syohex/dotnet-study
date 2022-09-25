let sortPeople (names: string list) (heights: int list) : string list =
    List.zip names heights
    |> List.sortWith (fun (_, height1) (_, height2) -> compare height2 height1)
    |> List.map (fun (name, _) -> name)

// ["Mary","Emma","John"]
sortPeople [ "Marry"; "John"; "Emma" ] [
    180
    165
    170
]

// ["Bob","Alice","Bob"]
sortPeople [ "Alice"; "Bob"; "Bob" ] [
    155
    185
    150
]
