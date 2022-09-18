let trap (height: int list) : int =
    let rec heightMax height (prev: int) acc =
        match height with
        | [] -> acc |> List.tail |> List.rev |> List.tail
        | h :: t ->
            let max = System.Math.Max(h, prev)
            heightMax t max (max :: acc)

    let rec trap' (leftMax: int list) rightMax height acc =
        match leftMax, rightMax, height with
        | [], [], _ -> acc
        | h1 :: t1, h2 :: t2, h3 :: t3 -> trap' t1 t2 t3 (acc + System.Math.Min(h1, h2) - h3)
        | _, _, _ -> failwith "never reach here"

    let leftMax =
        heightMax (List.tail height) (List.head height) [ (List.head height) ]

    let reversed = height |> List.rev

    let rightMax =
        heightMax (List.tail reversed) (List.head reversed) [ (List.head reversed) ]
        |> List.rev

    printf "%A => %A\n" leftMax rightMax
    trap' leftMax rightMax (height |> List.tail) 0

let height1 = [ 0; 1; 0; 2; 1; 0; 1; 3; 2; 1; 2; 1 ]
// 6
trap height1

// 9
trap [ 4; 2; 0; 3; 2; 5 ]
