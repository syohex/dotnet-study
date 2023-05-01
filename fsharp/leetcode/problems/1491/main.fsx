let average (salary: int list) : double =
    let len = List.length salary

    (salary |> List.sort |> List.take (len - 1) |> List.skip 1 |> List.sum |> double)
    / (len - 2 |> double)

// 2500.0
average [ 4000; 3000; 1000; 2000 ]

// 2000.0
average [ 1000; 2000; 3000 ]
