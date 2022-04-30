let toGraph (equations: (string * string) list) (values: double list) : Map<string, Map<string, double>> =
    let rec toGraph' equations values acc =
        match equations, values with
        | [], [] -> acc
        | (numerator, denominator) :: t1, value :: t2 ->
            let numMap =
                match Map.tryFind numerator acc with
                | None -> Map.empty
                | Some (m) -> m

            let denMap =
                match Map.tryFind denominator acc with
                | None -> Map.empty
                | Some (m) -> m

            let acc' =
                acc
                |> Map.add numerator (Map.add denominator value numMap)
                |> Map.add denominator (Map.add numerator (1.0 / value) denMap)

            toGraph' t1 t2 acc'
        | _ -> failwith "never reach here"

    toGraph' equations values Map.empty

let calcValue (current: string) (dest: string) (graph: Map<string, Map<string, double>>) : double =
    let rec calcValue'
        (current: string)
        (dest: string)
        (graph: Map<string, Map<string, double>>)
        (used: Set<string>)
        (acc: double)
        =
        let adjacents = Map.find current graph
        let used' = Set.add current used

        match Map.tryFind dest adjacents with
        | Some (v) -> acc * v
        | None ->
            let ret =
                adjacents
                |> Map.tryPick (fun k v ->
                    if Set.contains k used then
                        None
                    else
                        let c = calcValue' k dest graph used' (acc * v)
                        if c = 1.0 then None else Some(c))

            match ret with
            | Some (v) -> v
            | None -> -1.0

    calcValue' current dest graph Set.empty 1

let calcEquation
    (equations: (string * string) list)
    (values: double list)
    (queries: (string * string) list)
    : double list =

    let graph = toGraph equations values

    queries
    |> List.map (fun (numerator, denominator) ->
        if (Map.tryFind numerator graph |> Option.isNone)
           || (Map.tryFind denominator graph |> Option.isNone) then
            -1.0
        elif numerator = denominator then
            1.0
        else
            calcValue numerator denominator graph)

let equations1 = [ ("a", "b"); ("b", "c") ]
let values1 = [ 2.0; 3.0 ]

let queries1 =
    [ ("a", "c")
      ("b", "a")
      ("a", "e")
      ("a", "a")
      ("x", "x") ]

// [6.0, 0.5, -1.0, 1.0, -1.0]
calcEquation equations1 values1 queries1

let equations2 = [ ("a", "b"); ("b", "c"); ("bc", "cd") ]
let values2 = [ 1.5; 2.5; 5.0 ]

let queries2 =
    [ ("a", "c")
      ("c", "b")
      ("bc", "cd")
      ("cd", "bc") ]

// [3.75,0.4,5.0,0.2]
calcEquation equations2 values2 queries2

let equations3 = [ ("a", "b") ]
let values3 = [ 0.5 ]

let queries3 =
    [ ("a", "b")
      ("b", "a")
      ("a", "c")
      ("x", "y") ]

// [0.5, 2.0, -1, -1]
calcEquation equations3 values3 queries3
