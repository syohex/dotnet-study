let destCity (paths: (string * string) list) : string =
    let rec destCity' cities froms =
        match cities with
        | [] -> failwith "never reach here"
        | city :: t -> if Set.contains city froms then destCity' t froms else city

    let cities = paths |> List.fold (fun acc (a, b) -> b :: a :: acc) []

    let froms = paths |> List.map fst |> Set.ofList
    destCity' cities froms

// "Sao Paulo"
destCity [ ("London", "New York"); ("New York", "Lima"); ("Lima", "Sao Paulo") ]

// A
destCity [ ("B", "C"); ("D", "B"); ("C", "A") ]

// Z
destCity [ ("A", "Z") ]
