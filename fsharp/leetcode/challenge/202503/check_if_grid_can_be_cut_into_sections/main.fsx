let f (rectangles: int[] list) index =
    let rec f' (rectangles: int[] list) index maxEnd acc =
        match rectangles with
        | [] -> acc >= 2
        | h :: t ->
            let acc = if h.[index] >= maxEnd then acc + 1 else acc
            let maxEnd = max maxEnd (h.[index + 2])
            f' t index maxEnd acc

    let sorted =
        List.sortWith (fun (a: int[]) (b: int[]) -> compare a.[index] b.[index]) rectangles

    match sorted with
    | [] -> failwith "never reach here"
    | h :: t -> f' t index h.[index + 2] 0

let checkValidCuts (_n: int) (rectangles: int[] list) = f rectangles 0 || f rectangles 1

// true
checkValidCuts 5 [ [| 1; 0; 5; 2 |]; [| 0; 2; 2; 4 |]; [| 3; 2; 5; 3 |]; [| 0; 4; 4; 5 |] ]

// true
checkValidCuts 4 [ [| 0; 0; 1; 1 |]; [| 2; 0; 3; 4 |]; [| 0; 2; 2; 3 |]; [| 3; 0; 4; 3 |] ]

// false
checkValidCuts
    4
    [ [| 0; 2; 2; 4 |]
      [| 1; 0; 3; 2 |]
      [| 2; 2; 3; 4 |]
      [| 3; 0; 4; 2 |]
      [| 3; 2; 4; 4 |] ]
