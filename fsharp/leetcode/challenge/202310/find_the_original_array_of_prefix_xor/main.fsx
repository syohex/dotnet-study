let findArray (pref: int list) : int list =
    let head = List.head pref

    List.tail pref
    |> List.fold
        (fun (ret, acc) n ->
            let m = acc ^^^ n
            m :: ret, acc ^^^ m)
        ([ head ], head)
    |> fst
    |> List.rev

// [5;7;2;3;2]
findArray [ 5; 2; 0; 3; 1 ]

// [13]
findArray [ 13 ]
