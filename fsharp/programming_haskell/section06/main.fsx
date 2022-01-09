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

isort [ 3; 2; 1; 4 ]

// 6.3
let rec zip xs ys =
    match (xs, ys) with
    | ([], [])
    | ([], _)
    | (_, []) -> []
    | (h1 :: t1, h2 :: t2) -> (h1, h2) :: zip t1 t2

zip [ 'a'; 'b'; 'c' ] [ 1; 2; 3; 4 ]

let rec drop n xs =
    if n = 0 then
        xs
    else
        match xs with
        | [] -> []
        | h :: t -> drop (n - 1) t

drop 3 [ 1; 2; 3; 4; 5; 6; 7 ]

// 6.4
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

fib 10
fib 5
seq { 1 .. 10 } |> Seq.map fib |> Seq.toList

let rec qsort ns =
    match ns with
    | [] -> []
    | head :: tail ->
        let lowers = tail |> List.filter (fun n -> n < head)
        let uppers = tail |> List.filter (fun n -> n >= head)
        qsort lowers @ [ head ] @ qsort uppers

qsort [ 1; 5; 3; 3; 4; 4; 4; 333 ]

// 6.5
let rec even n =
    match n with
    | 0 -> true
    | n -> odd (n - 1)

and odd n =
    match n with
    | 0 -> false
    | n -> even (n - 1)

even 4
odd 4

let rec evens xs =
    match xs with
    | [] -> []
    | h :: t -> h :: odds t

and odds xs =
    match xs with
    | [] -> []
    | _ :: t -> evens t

"abcde" |> Seq.toList |> odds
"abcde" |> Seq.toList |> evens

// 6.8
let rec sumDown n =
    match n with
    | 0 -> 0
    | n -> n + sumDown (n - 1)

sumDown 3

let rec power n p =
    match p with
    | 0 -> 1
    | p -> n * power n (p - 1)

power 2 3

let rec euclid a b =
    if a = b then a
    elif a > b then euclid b (a - b)
    else euclid a (b - a)

euclid 6 27

let rec myAnd xs =
    match xs with
    | [] -> true
    | h :: _ when h = false -> false
    | _ :: t -> myAnd t

myAnd [ true; false ]
myAnd [ true; true ]

let rec concat xss =
    match xss with
    | [] -> []
    | h :: t -> h @ concat t

concat [ [ 1; 2; 3 ]
         [ 4; 5; 6 ]
         [ 7; 8; 9 ] ]

let rec replicate n x =
    if n = 0 then
        []
    else
        x :: replicate (n - 1) x

replicate 3 'b'
replicate 10 [ 1 ]

let rec getByIndex n xs =
    if n = 0 then
        List.head xs
    else
        getByIndex (n - 1) (List.tail xs)

getByIndex 1 [ 1; 2; 3 ]
getByIndex 0 [ 1; 2; 3 ]

let rec elem x xs =
    match xs with
    | [] -> false
    | h :: t -> if h = x then true else elem x t

elem 2 [ 1; 2; 3 ]
elem 2 [ 1; 4; 3 ]

let rec merge xs ys =
    match (xs, ys) with
    | ([], []) -> []
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (h1 :: t1, h2 :: t2) ->
        if h1 < h2 then
            h1 :: merge t1 ys
        else
            h2 :: merge xs t2

merge [ 2; 5; 6 ] [ 1; 3; 4 ]

let rec halve xs =
    let n = (xs |> List.length) / 2
    ((xs |> List.take n), (xs |> List.skip n))

let rec mergeSort xs =
    match xs with
    | [] -> []
    | a :: [] -> [ a ]
    | _ ->
        let ys = halve xs
        merge (mergeSort (fst ys)) (mergeSort (snd ys))

mergeSort [ 3; 4; 2; 1; 5 ]

let rec sum xs =
    match xs with
    | [] -> 0
    | h :: t -> h + sum t

sum [ 1; 2; 3 ]

let rec take n xs =
    match (n, xs) with
    | (0, _) -> []
    | (_, []) -> []
    | (n, h :: t) -> h :: take (n - 1) t

seq { 1 .. 10 } |> Seq.toList |> take 3

let rec last xs =
    match xs with
    | [] -> failwith "invalid"
    | a :: [] -> a
    | _ :: t -> last t

last [ 1; 2; 3 ]
