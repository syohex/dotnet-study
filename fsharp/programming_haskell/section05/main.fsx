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

// 5.5
let let2int (c: char) : int = int c - int 'a'

let int2let (n: int) : char = char (int 'a' + n)

let2int 'a'
int2let 0

let shift (n: int) (c: char) =
    match c with
    | c when c >= 'a' && c <= 'z' -> int2let ((let2int c + n) % 26)
    | c -> c

shift 3 'a'
shift 3 'z'

let encode (n: int) (s: string) : string =
    s
    |> Seq.toList
    |> Seq.map (fun c -> shift n c)
    |> System.String.Concat

encode 3 "haskell is fun"

// 5.7
let grid x y =
    let xs = seq { 0 .. x }
    let ys = seq { 0 .. y }

    let mutable ret: (int * int) list = []

    for a in xs do
        for b in ys do
            ret <- (a, b) :: ret

    ret |> List.rev

grid 1 2

let square n = grid n n

square 2

let replicate (n: int) v =
    let rec replicate' (m: int) (n: int) v =
        if m = n then
            []
        else
            v :: replicate' (m + 1) n v

    replicate' 0 n v

replicate 3 true
replicate 3 "foo"

let pyths n =
    let xs = seq { 1 .. n }
    let ys = seq { 1 .. n }
    let zs = seq { 1 .. n }

    let mutable ret = []

    for x in xs do
        for y in ys do
            for z in zs do
                if (x * x) + (y * y) = (z * z) then
                    ret <- (x, y, z) :: ret

    ret |> List.rev

pyths 10

let perfects n =
    seq { 1 .. n }
    |> Seq.filter (fun m -> (factors m |> Seq.sum) = m)
    |> Seq.toList

factors 6
perfects 500
