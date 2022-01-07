let even n = n % 2 = 0

let splitAt n xs = ((List.take n xs), (List.skip n xs))

splitAt 3 [ 1; 2; 3; 4; 5 ]

let recip n = 1.0 / n

let abs n = if n >= 0 then n else -n

// 4.8

/// 4.8.1
let halve xs =
    let num = (List.length xs / 2)
    ((List.take num xs), (List.skip num xs))

halve [ 1; 2; 3; 4; 5; 6 ]

/// 4.8.2
let third1 xs =
    xs |> List.tail |> List.tail |> List.head

third1 [ 1; 2; 3; 4 ]

let third2 xs = xs |> List.item 2

third2 [ 1; 2; 3; 4 ]

let third3 xs =
    match xs with
    | _ :: _ :: n :: _ -> n
    | _ -> failwith "invalid input"

third3 [ 1; 2; 3; 4 ]

/// 4.8.3
let safeTail1 xs =
    if List.isEmpty xs then
        []
    else
        List.tail xs

let safeTail2 xs =
    match xs with
    | [] -> []
    | _ :: tail -> tail

safeTail1 [ 1 ]
safeTail2 [ 1 ]

/// 4.8.8
let luhn (ns: int list) : bool =
    let luhnDouble n =
        let v = n * 2
        if v > 9 then v - 9 else v

    let rec luhn' (ns: int list) (n: int) (acc: int) : bool =
        match ns with
        | [] -> acc % 10 = 0
        | x :: xs when n % 2 = 0 -> luhn' xs (n + 1) ((luhnDouble x) + acc)
        | x :: xs -> luhn' xs (n + 1) (x + acc)

    luhn' (List.rev ns) 1 0

luhn [ 1; 7; 8; 4 ]
luhn [ 4; 7; 8; 3 ]
