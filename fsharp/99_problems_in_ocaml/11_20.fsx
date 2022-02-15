// problem 11
type Rle<'a> =
    | One of 'a
    | Many of int * 'a

let encode2 xs =
    let rec encode2' xs prev count acc =
        match xs with
        | [] ->
            if count = 1 then
                ((One prev) :: acc) |> List.rev
            else
                ((Many(count, prev)) :: acc) |> List.rev
        | y :: ys ->
            if prev = y then
                encode2' ys prev (count + 1) acc
            else if count = 1 then
                encode2' ys y 1 ((One prev) :: acc)
            else
                encode2' ys y 1 ((Many(count, prev)) :: acc)

    match xs with
    | [] -> []
    | y :: ys -> encode2' ys y 1 []

//[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
// Many (4, "e")]
encode2 [ "a"
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

// problem 12
let decode xs =
    let rec decode' xs acc =
        match xs with
        | [] -> acc |> List.rev
        | y :: ys ->
            match y with
            | One v -> decode' ys (v :: acc)
            | Many (count, c) ->
                let zs = List.init count (fun _ -> c)
                decode' ys (zs @ acc)

    decode' xs []

// ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
decode [ Many(4, "a")
         One "b"
         Many(2, "c")
         Many(2, "a")
         One "d"
         Many(4, "e") ]

// problem 13
let encode3 xs =
    let rec encode3' xs prev count acc =
        match xs with
        | [] ->
            if count = 1 then
                ((One prev) :: acc) |> List.rev
            else
                ((Many(count, prev)) :: acc) |> List.rev
        | y :: ys ->
            if prev = y then
                encode3' ys prev (count + 1) acc
            else if count = 1 then
                encode3' ys y 1 ((One prev) :: acc)
            else
                encode3' ys y 1 ((Many(count, prev)) :: acc)

    match xs with
    | [] -> []
    | y :: ys -> encode3' ys y 1 []

// [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
//  Many (4, "e")]
encode3 [ "a"
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

// problem 14
let duplicate xs =
    let rec duplicate' xs acc =
        match xs with
        | [] -> acc |> List.rev
        | y :: ys -> duplicate' ys (y :: y :: acc)

    duplicate' xs []

// ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
duplicate [ "a"; "b"; "c"; "c"; "d" ]

// problem 15
let replicate xs n =
    let rec replicate' xs n acc =
        match xs with
        | [] -> acc |> List.rev
        | y :: ys ->
            let newAcc =
                List.init n id
                |> List.fold (fun acc _ -> y :: acc) acc

            replicate' ys n newAcc

    replicate' xs n []

// ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
replicate [ "a"; "b"; "c" ] 3
