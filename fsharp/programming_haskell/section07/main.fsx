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
