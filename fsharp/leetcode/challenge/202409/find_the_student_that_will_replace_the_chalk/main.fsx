let chalkReplacer (chalk: int list) (k: int) : int =
    let rec chalkReplacer' (chalk: (int * int64) list) (k: int64) =
        match chalk with
        | [] -> failwith "never reach here"
        | (i, h) :: t -> if h > k then i else chalkReplacer' t (k - h)

    let chalk = List.map int64 chalk
    let sum = chalk |> List.sum
    chalkReplacer' (List.indexed chalk) (int64 k % sum)

// 0
chalkReplacer [ 5; 1; 5 ] 22

// 1
chalkReplacer [ 3; 4; 1; 2 ] 25
