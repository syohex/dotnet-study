let findArray (pref: int list) : int list =
    let rec findArray' pref acc ret =
        match pref with
        | [] -> ret |> List.rev
        | h :: t ->
            let v = acc ^^^ h
            findArray' t (acc ^^^ v) (v :: ret)

    findArray' (List.tail pref) (List.head pref) [ List.head pref ]

// [5,7,2,3,2]
findArray [ 5; 2; 0; 3; 1 ]

// [13]
findArray [ 13 ]
