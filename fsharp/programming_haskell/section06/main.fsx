// 6.1
let factorial1 n =
    seq { 1 .. n }
    |> Seq.fold (fun acc m -> acc * m) 1

factorial1 10

let rec factorial2 n =
    match n with
    | 0 -> 1
    | m -> m * factorial2 (m - 1)

factorial2 10

// 6.2
let rec product ns =
    match ns with
    | [] -> 1
    | head :: tail -> head * product tail

product [ 1; 2; 3; 4; 5; 6 ]

let rec length xs =
    match xs with
    | [] -> 0
    | _ :: tail -> 1 + length tail

length [ 1; 2; 3; 4; 5 ]

let rec reverse1 xs =
    match xs with
    | [] -> []
    | head :: tail -> reverse1 tail @ [ head ]

reverse1 [ 1; 2; 3; 4 ]

let reverse2 xs =
    let rec reverse2' xs acc =
        match xs with
        | [] -> acc
        | head :: tail -> reverse2' tail (head :: acc)

    reverse2' xs []

reverse2 [ 1; 2; 3; 4 ]

let rec insert n ns =
    match ns with
    | [] -> [ n ]
    | head :: tail when n < head -> n :: head :: tail
    | head :: tail -> head :: insert n tail

insert 3 [ 1; 2; 4; 5 ]

let rec isort ns =
    match ns with
    | [] -> []
    | head :: tail -> insert head (isort tail)

isort [3;2;1;4]