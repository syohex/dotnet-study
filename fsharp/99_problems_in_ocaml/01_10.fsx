// problem 01
let last<'a> (xs: 'a list) =
    match xs with
    | [] -> None
    | _ -> xs |> List.rev |> List.head |> Some

// None
last<string> []

// Some "d"
last [ "a"; "b"; "c"; "d" ]

// problem 02
let rec lastTwo<'a> (xs: 'a list) =
    match xs with
    | []
    | [ _ ] -> None
    | [ a; b ] -> Some(a, b)
    | _ :: tail -> lastTwo tail

// None
lastTwo [ "a" ]

// Some ("c", "d")
lastTwo [ "a"; "b"; "c"; "d" ]

// problem 03
let at n xs =
    let rec at' i n xs =
        match xs with
        | [] -> None
        | head :: tail ->
            if i = n then
                Some head
            else
                at' (i + 1) n tail

    at' 1 n xs

// Some "c"
at 3 [ "a"; "b"; "c"; "d"; "e" ]

// None
at 3 [ "a" ]

// problem 04
let length xs =
    let rec length' xs n =
        match xs with
        | [] -> n
        | _ :: tail -> length' tail (n + 1)

    length' xs 0

// 3
length [ "a"; "b"; "c" ]

// 0
length []

// problem 05
let reverse xs =
    let rec reverse' xs acc =
        match xs with
        | [] -> acc
        | head :: tail -> reverse' tail (head :: acc)

    reverse' xs []

// ["c", "b", "a"]
reverse [ "a"; "b"; "c" ]

// problem 06
let isPalindrome xs : bool =
    let rec equal xs ys =
        match xs, ys with
        | [], [] -> true
        | _, []
        | [], _ -> false
        | x :: tail1, y :: tail2 ->
            if x <> y then
                false
            else
                equal tail1 tail2

    equal xs (xs |> List.rev)

// true
isPalindrome [ "x"; "a"; "m"; "a"; "x" ]

// false
isPalindrome [ "a"; "b" ]

// problem 7
type Node<'a> =
    | One of 'a
    | Many of Node<'a> list

let rec flatten xs =
    match xs with
    | [] -> []
    | x :: tail ->
        match x with
        | One v -> v :: (flatten tail)
        | Many ys -> (flatten ys) @ (flatten tail)

// ["a";"b";"c";"d";"e"]
flatten [ One "a"
          Many [ One "b"
                 Many [ One "c"; One "d" ]
                 One "e" ] ]

// problem 8
let compress xs =
    let rec compress' xs prev acc =
        match xs with
        | [] -> (prev :: acc) |> List.rev
        | y :: ys ->
            if prev = y then
                compress' ys prev acc
            else
                compress' ys y (prev :: acc)

    match xs with
    | [] -> []
    | y :: ys -> compress' ys y []

// ["a"; "b"; "c"; "a"; "d"; "e"]
compress [ "a"
           "a"
           "a"
           "a"
           "b"
           "c"
           "c"
           "a"
           "a"
           "d"
           "e"
           "e"
           "e"
           "e" ]

// problem 9
let pack xs =
    let rec pack' xs prev tmp acc =
        match xs with
        | [] -> (tmp :: acc) |> List.rev
        | y :: ys ->
            if prev = y then
                pack' ys prev (prev :: tmp) acc
            else
                pack' ys y [ y ] (tmp :: acc)

    match xs with
    | [] -> []
    | y :: ys -> pack' ys y [ y ] []


// [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
pack [ "a"
       "a"
       "a"
       "a"
       "b"
       "c"
       "c"
       "a"
       "a"
       "d"
       "d"
       "e"
       "e"
       "e"
       "e" ]

// problem 10
let encode xs =
    let rec encode' xs prev count acc =
        match xs with
        | [] -> ((count, prev) :: acc) |> List.rev
        | y :: ys ->
            if prev = y then
                encode' ys prev (count + 1) acc
            else
                encode' ys y 1 ((count, prev) :: acc)

    match xs with
    | [] -> []
    | y :: ys -> encode' ys y 1 []

// [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
encode [ "a"
         "a"
         "a"
         "a"
         "b"
         "c"
         "c"
         "a"
         "a"
         "d"
         "e"
         "e"
         "e"
         "e" ]
