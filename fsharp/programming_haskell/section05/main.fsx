// 5.1
let rec concat xss =
    match xss with
    | [] -> []
    | xs :: xss -> xs @ concat xss

concat [ [ 1; 2; 3 ]
         [ 4; 5; 6 ]
         [ 7; 8; 9 ] ]

let rec firsts xs = xs |> List.map fst

firsts [ (1, 2); (3, 4); (5, 6) ]

let length xs = xs |> List.map (fun _ -> 1) |> List.sum

length [ 1; 2; 3 ]

// 5.2
let factors n =
    seq { 1 .. n }
    |> Seq.filter (fun m -> n % m = 0)
    |> Seq.toList

factors 9
factors 18
factors 15

let prime n =
    match factors n with
    | [ 1; n ] -> true
    | _ -> false

let primes n =
    seq { 2 .. n } |> Seq.filter prime |> Seq.toList

primes 40

let find x xs =
    xs
    |> List.filter (fun n -> fst n = x)
    |> List.map snd

find
    'b'
    [ ('a', 1)
      ('b', 2)
      ('c', 3)
      ('b', 4) ]

// 5.3
let rec zip a b =
    match (a, b) with
    | ([], [])
    | ([], _)
    | (_, []) -> []
    | (x1 :: tail1, x2 :: tail2) -> (x1, x2) :: zip tail1 tail2

zip [ 'a'; 'b'; 'c' ] [ 1; 2; 3; 4 ]

let pairs xs = zip xs (List.tail xs)

pairs [ 1; 2; 3; 4 ]

let sorted xs =
    xs |> pairs |> List.forall (fun (x, y) -> x <= y)

sorted [ 1; 2; 3; 4 ]
sorted [ 1; 2; 5; 4 ]

let positions x xs =
    let rec positions' x xs pos acc =
        match xs with
        | [] -> List.rev acc
        | head :: tail ->
            if head = x then
                positions' x tail (pos + 1) (pos :: acc)
            else
                positions' x tail (pos + 1) acc

    positions' x xs 0 []

positions false [ true; false; true; false ]

// 5.4
let lowers s =
    s
    |> Seq.filter (fun c -> c >= 'a' && c <= 'z')
    |> Seq.length

lowers "HellO"

let count c s =
    s |> Seq.filter (fun x -> x = c) |> Seq.length

count 'l' "hello"
