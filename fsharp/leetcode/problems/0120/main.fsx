open System

let minimumTotal (triangle: int list list) : int =
    let rec minimumTotal' triangle (prev: int []) =
        match triangle with
        | [] -> prev |> Array.min
        | h :: t ->
            let sum =
                h
                |> List.mapi (fun i n ->
                    if i = 0 then
                        n + prev.[0]
                    elif i = prev.Length then
                        n + prev.[prev.Length - 1]
                    else
                        Math.Min(prev.[i - 1], prev.[i]) + n)
                |> List.toArray

            minimumTotal' t sum

    minimumTotal' triangle.Tail (triangle.Head |> List.toArray)

let triangle1 =
    [ [ 2 ]
      [ 3; 4 ]
      [ 6; 5; 7 ]
      [ 4; 1; 8; 3 ] ]

// 11
minimumTotal triangle1

// -10
minimumTotal [ [ -10 ] ]
