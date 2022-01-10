// 7.1
let twice f = f >> f
let twice2 f x = f (f x)

twice ((+) 10) 5
twice2 ((+) 10) 5

// 7.2
let myMap f xs = seq { for x in xs -> f x }

myMap ((+) 1) [ 1; 2; 3; 4 ]

let rec myFilter f xs =
    match xs with
    | [] -> []
    | h :: t ->
        if f h then
            h :: myFilter f t
        else
            myFilter f t

myFilter (fun n -> n % 2 = 0) [ 1 .. 10 ]
myFilter ((<) 5) [ 1 .. 10 ]

let rec all f xs =
    match xs with
    | [] -> true
    | h :: _ when not (f h) -> false
    | _ :: t -> all f t

all (fun n -> n % 2 = 0) [ 2; 4; 6; 8 ]
all (fun n -> n % 2 = 0) [ 1; 3 ]
all (fun n -> n % 2 = 0) [ 1; 2 ]

let rec any f xs =
    match xs with
    | [] -> false
    | h :: _ when f h -> true
    | _ :: t -> any f t

any (fun n -> n % 2 = 0) [ 2; 4; 6; 8 ]
any (fun n -> n % 2 = 0) [ 1; 3 ]
any (fun n -> n % 2 = 0) [ 1; 2 ]

let rec takeWhile f xs =
    match xs with
    | [] -> []
    | h :: _ when not (f h) -> []
    | h :: t -> h :: takeWhile f t

takeWhile (fun n -> n % 2 = 0) [ 2; 4; 6; 7; 10 ]

let rec dropWhile f xs =
    match xs with
    | [] -> []
    | h :: t when not (f h) -> xs
    | _ :: t -> dropWhile f t

dropWhile (fun n -> n % 2 = 1) [ 1; 3; 5; 6; 7 ]

// 7.3
let rec foldr f v xs =
    match xs with
    | [] -> v
    | h :: t -> f h (foldr f v t)

let sum xs = foldr (+) 0 xs

sum [ 1 .. 100 ]

// 7.4
let rec foldl f v xs =
    match xs with
    | [] -> v
    | h :: t -> foldl f (f v h) t

let length xs = foldl (fun acc _ -> 1 + acc) 0 xs

length [ 1 .. 100 ]

// 7.6
let bin2int xs =
    xs
    |> List.rev
    |> List.fold (fun acc n -> (2 * acc) + n) 0

bin2int [ 1; 0; 1; 1 ]

let rec int2bin n =
    if n = 0 then
        []
    else
        (n % 2) :: int2bin (n / 2)

int2bin 13

// 7.9
let myMap2 f xs = foldr (fun x acc -> (f x) :: acc) [] xs

myMap2 ((+) 1) [ 1 .. 10 ]

let myFilter2 f xs =
    foldr (fun x acc -> if f x then x :: acc else acc) [] xs

myFilter2 (fun n -> n % 2 = 0) [ 1 .. 10 ]

let dec2int ns =
    ns |> List.fold (fun acc n -> 10 * acc + n) 0

dec2int [ 2; 3; 4; 5 ]
