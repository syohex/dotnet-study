let rec nth n xs =
    if n = 0 then
        List.head xs
    else
        nth (n - 1) (List.tail xs)

let collectCol xss n = xss |> List.map (nth n)

let valid (len: int) (xs: int list) : bool =
    let x =
        xs
        |> List.fold (fun s x -> Set.add x s) Set.empty
        |> Set.count

    x = len

let checkValid (matrix: int list list) : bool =
    let len = List.length matrix

    let cols =
        [ 1 .. (len - 1) ] |> List.map (collectCol matrix)

    let b1 =
        matrix |> List.map (valid len) |> List.forall id

    let b2 =
        cols |> List.map (valid len) |> List.forall id

    b1 && b2

let matrix1 =
    [ [ 1; 2; 3 ]
      [ 3; 1; 2 ]
      [ 2; 3; 1 ] ]

checkValid matrix1

let matrix2 =
    [ [ 1; 1; 1 ]
      [ 1; 2; 3 ]
      [ 1; 2; 3 ] ]

checkValid matrix2
