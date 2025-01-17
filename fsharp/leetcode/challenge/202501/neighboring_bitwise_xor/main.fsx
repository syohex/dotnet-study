let doesValidArrayExist (derived: int list) : bool =
    let ret1 = derived |> List.fold (fun prev n -> prev ^^^ n) 0
    let ret2 = derived |> List.fold (fun prev n -> prev ^^^ n) 1
    ret1 = 0 || ret2 = 1

// true
doesValidArrayExist [ 1; 1; 0 ]

// true
doesValidArrayExist [ 1; 1 ]

// false
doesValidArrayExist [ 1; 0 ]
