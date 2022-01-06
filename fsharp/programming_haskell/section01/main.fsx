// 1.5
let rec sum (ns: int list) : int =
    match ns with
    | [] -> 0
    | n :: tail -> n + sum tail

sum [ 1; 2; 3 ]

// 1.5.2
let rec qsort (ns: int list) : int list =
    match ns with
    | [] -> []
    | n :: tail ->
        let smalls = tail |> List.filter (fun m -> m <= n)
        let larges = tail |> List.filter (fun m -> m > n)
        qsort smalls @ [ n ] @ qsort larges

qsort [ 5; 4; 3; 2; 1 ]

// 1.7 exercise

// 1.7.3
let rec product (nums: int list) : int =
    match nums with
    | [] -> 1
    | n :: ns -> n * product ns

product [ 1; 2; 3; 4 ]

// 1.7.4
let rec qsortDesc (ns: int list) : int list =
    match ns with
    | [] -> []
    | n :: tail ->
        let larges = tail |> List.filter (fun m -> m > n)
        let smalls = tail |> List.filter (fun m -> m <= n)
        qsortDesc larges @ [ n ] @ qsortDesc smalls

qsortDesc [ 1; 2; 3; 4; 5 ]
