let findChampion (grid: int list list) : int =
    let len = List.length grid

    grid
    |> List.map (fun v -> v |> List.filter ((=) 1) |> List.length)
    |> List.findIndex (fun n -> n = len - 1)


// 0
findChampion [ [ 0; 1 ]; [ 0; 0 ] ]

// 1
findChampion [ [ 0; 0; 1 ]; [ 1; 0; 1 ]; [ 0; 0; 0 ] ]
