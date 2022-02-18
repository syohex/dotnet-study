// problem 21
let rec insertAt x n xs =
    match xs with
    | [] -> [ x ]
    | y :: ys ->
        if n = 1 then
            y :: x :: ys
        else
            y :: (insertAt x (n - 1) ys)


// ["a"; "alfa"; "b"; "c"; "d"]
insertAt "alfa" 1 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "alfa"; "d"]
insertAt "alfa" 3 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "d"; "alfa"]
insertAt "alfa" 4 [ "a"; "b"; "c"; "d" ]

// ["a"; "b"; "c"; "d"; "x"]
insertAt "x" 100 [ "a"; "b"; "c"; "d" ]

// problem 22
let range m n =
    let rec range' m n acc =
        if m > n then
            acc |> List.rev
        else
            range' (m + 1) n (m :: acc)

    if m < n then
        range' m n []
    else
        range' n m [] |> List.rev

// [4; 5; 6; 7; 8; 9]
range 4 9

// [9; 8; 7; 6; 5; 4]
range 9 4

// problem 23
let randSelect<'a> xs n =
    let rec randSelect' i (r: System.Random) (xs: 'a []) n acc =
        if i = n then
            acc
        else
            let m = r.Next(0, (n - i))
            let idx = (Array.length xs) - i - 1
            let tmp = xs.[idx]
            xs.[idx] <- xs.[m]
            xs.[m] <- tmp
            randSelect' (i + 1) r xs n (xs.[idx] :: acc)

    let ys = xs |> List.toArray
    randSelect' 0 (System.Random()) ys n []

randSelect
    [ "a"
      "b"
      "c"
      "d"
      "e"
      "f"
      "g"
      "h" ]
    3

// problem 24
let lottoSelect n m =
    let rec lottoSelect' n m (rnd: System.Random) used acc =
        if n = 0 then
            acc
        else
            let v = rnd.Next(m + 1)

            if Set.contains v used then
                lottoSelect' n m rnd used acc
            else
                lottoSelect' (n - 1) m rnd (Set.add v used) (v :: acc)

    lottoSelect' n m (System.Random()) Set.empty []

lottoSelect 6 49

// problem 25
let permutation xs =
    let rec permutation' i xs (rnd: System.Random) acc =
        let len = Array.length xs

        if i = len then
            acc
        else
            let v = rnd.Next(len - i)
            let last = len - i - 1
            let tmp = xs.[last]
            xs.[last] <- xs.[v]
            xs.[v] <- tmp
            permutation' (i + 1) xs rnd (xs.[last] :: acc)

    permutation' 0 (xs |> List.toArray) (System.Random()) []

permutation [ "a"
              "b"
              "c"
              "d"
              "e"
              "f" ]

// problem 26
let rec extract<'a> (n: int) (xs: 'a list) : 'a list list =
    if n <= 0 then
        [ [] ]
    else
        match xs with
        | [] -> []
        | y :: ys ->
            let ps =
                List.map (fun a -> y :: a) (extract (n - 1) ys)

            let qs = extract n ys
            ps @ qs

extract 2 [ "a"; "b"; "c"; "d" ]

// TODO problem 27

// problem 28
let lengthSort<'a> (xss: 'a list list) = xss |> List.sortBy List.length

// [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
//  ["i"; "j"; "k"; "l"]]
lengthSort [ [ "a"; "b"; "c" ]
             [ "d"; "e" ]
             [ "f"; "g"; "h" ]
             [ "d"; "e" ]
             [ "i"; "j"; "k"; "l" ]
             [ "m"; "n" ]
             [ "o" ] ]

let frequencySort<'a> (xss: 'a list list) =
    let freq =
        xss
        |> List.map List.length
        |> List.fold
            (fun m n ->
                match Map.tryFind n m with
                | None -> Map.add n 1 m
                | Some v -> Map.add n (v + 1) m)
            Map.empty

    xss
    |> List.sortBy (fun xs -> Map.find (List.length xs) freq)

// [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
//  ["d"; "e"]; ["m"; "n"]]
frequencySort [ [ "a"; "b"; "c" ]
                [ "d"; "e" ]
                [ "f"; "g"; "h" ]
                [ "d"; "e" ]
                [ "i"; "j"; "k"; "l" ]
                [ "m"; "n" ]
                [ "o" ] ]