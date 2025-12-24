let minimumBoxes (apple: int list) (capacity: int list) : int =
    let rec minimumBoxes' total capacity acc =
        match capacity with
        | [] -> acc
        | h :: t ->
            let total = total - h
            let acc = acc + 1
            if total <= 0 then acc else minimumBoxes' total t acc

    let total = List.sum apple
    let capacity = capacity |> List.sort |> List.rev
    minimumBoxes' total capacity 0

// 2
minimumBoxes [ 1; 3; 2 ] [ 4; 3; 1; 5; 2 ]

// 4
minimumBoxes [ 5; 5; 5 ] [ 2; 4; 2; 7 ]
