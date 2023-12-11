let findSpecialInteger (arr: int list) : int =
    arr
    |> List.windowed ((List.length arr / 4) + 1)
    |> List.find (fun a -> List.head a = List.last a)
    |> List.head

// 6
findSpecialInteger [1;2;2;6;6;6;6;7;10]

// 1
findSpecialInteger [1;1]
