let toMap (s: string) : Map<char, int> =
    s
    |> Seq.fold
        (fun acc c ->
            match Map.tryFind c acc with
            | Some(v) -> Map.add c (v + 1) acc
            | None -> Map.add c 1 acc)
        Map.empty

let findLastIndex (g: Map<char, int> list) (garbage: char) : int =
    g
    |> List.indexed
    |> List.fold (fun acc (i, m) -> if Map.containsKey garbage m then i else acc) -1

let garbageCollection (garbage: string list) (travel: int list) : int =
    let rec calculateCost gs garbage limit travel acc =
        match gs with
        | [] -> acc
        | (i, g) :: t ->
            let count = Map.tryFind garbage g |> Option.defaultValue 0
            let acc' = acc + count

            if i >= limit then
                acc'
            else
                calculateCost t garbage limit (List.tail travel) (acc' + List.head travel)

    let garbageTypes = [ 'M'; 'P'; 'G' ]
    let gs = garbage |> List.map toMap

    let lastIndexes =
        garbageTypes
        |> List.fold
            (fun acc c ->
                let index = findLastIndex gs c
                Map.add c index acc)
            Map.empty

    let gs' = List.indexed gs

    garbageTypes
    |> List.fold
        (fun acc garbage ->
            let limit = Map.find garbage lastIndexes
            acc + calculateCost gs' garbage limit travel 0)
        0

// 21
garbageCollection [ "G"; "P"; "GP"; "GG" ] [ 2; 4; 3 ]

// 37
garbageCollection [ "MMM"; "PGM"; "GP" ] [ 3; 10 ]
