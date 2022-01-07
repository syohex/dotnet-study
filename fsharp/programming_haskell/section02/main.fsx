// 2.7.4
let rec last xs =
    match xs with
    | [] -> failwith "invalid empty list"
    | x :: [] -> x
    | _ :: tail -> last tail

last [ 1; 2; 3; 4; 5 ]
last [ 1 ]
last []

let last2 xs =
    xs |> List.rev |> List.tail |> List.rev

last2 [ 1; 2; 3; 4; 5 ]
last2 [ 1 ]
last2 []

// 2.7.5
let init xs =
    let rec init' xs acc =
        match xs with
        | [] -> failwith "invalid empty list"
        | _ :: [] -> List.rev acc
        | x :: tail -> init' tail (x :: acc)

    init' xs []

init [ 1; 2; 3; 4; 5 ]
init [ 1 ]

let rec init2 xs =
    match xs with
    | [] -> failwith "invalid empty list"
    | _ :: [] -> []
    | x :: tail -> x :: (init2 tail)

init2 [ 1; 2; 3; 4; 5 ]
init2 [ 1 ]