open System

let numberOfPoints (nums: (int * int) list) : int =
    let rec numberOfPoints' nums (s: int) (e: int) acc =
        match nums with
        | [] -> acc + (e - s + 1)
        | (s1, e1) :: t ->
            if s1 > e then
                numberOfPoints' t s1 e1 (acc + e - s + 1)
            else
                let e' = Math.Max(e, e1)
                numberOfPoints' t s e' acc

    let nums' =
        List.sortWith (fun (s1, e1) (s2, e2) -> if s1 = s2 then compare e2 e1 else compare s1 s2) nums

    match nums' with
    | [] -> failwith "never reach here"
    | (s, e) :: t -> numberOfPoints' t s e 0

// 7
numberOfPoints [ (3, 6); (1, 5); (4, 7) ]

// 7
numberOfPoints [ (1, 3); (5, 8) ]

// 10
numberOfPoints [ (1, 10); (2, 8); (3, 7) ]
