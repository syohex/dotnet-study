let numberOfEmployeesWhoMetTarget (hours: int list) (target: int) : int =
    hours |> List.filter (fun n -> n >= target) |> List.length

// 3
numberOfEmployeesWhoMetTarget [ 0; 1; 2; 3; 4 ] 2 |> printfn "%A"

// 0
numberOfEmployeesWhoMetTarget [ 5; 1; 4; 2; 2 ] 6 |> printfn "%A"
