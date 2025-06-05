let toGraph (s1: string) (s2: string) : Map<char, char list> =
    let rec toGraph' cs1 cs2 acc =
        match cs1, cs2 with
        | [], [] -> acc
        | _, []
        | [], _ -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            let v1 = Map.tryFind h1 acc |> Option.defaultValue []
            let v2 = Map.tryFind h2 acc |> Option.defaultValue []

            let acc = acc |> Map.add h1 (h2 :: v1) |> Map.add h2 (h1 :: v2)
            toGraph' t1 t2 acc

    toGraph' (Seq.toList s1) (Seq.toList s2) Map.empty

let createSmallestTable (graph: Map<char, char list>) : Map<char, char> =
    let rec f c visited =
        let visited = Set.add c visited

        match Map.tryFind c graph with
        | None -> c, visited
        | Some(nexts) ->
            nexts
            |> List.fold
                (fun (acc, visited) next ->
                    if Set.contains next visited then
                        acc, visited
                    else
                        let acc', visited = f next visited
                        min acc acc', visited)
                (c, visited)

    seq { 0..25 }
    |> Seq.fold
        (fun acc i ->
            let c = char <| i + int 'a'
            let smallest, visited = f c Set.empty

            visited |> Set.fold (fun acc c -> Map.add c smallest acc) acc)
        Map.empty

let smallestEquivalentString (s1: string) (s2: string) (baseStr: string) : string =
    let graph = toGraph s1 s2
    let smallestTable = createSmallestTable graph

    baseStr |> Seq.map (fun c -> Map.find c smallestTable) |> System.String.Concat

// "makkek"
smallestEquivalentString "parker" "morris" "parser"

// "hdld"
smallestEquivalentString "hello" "world" "hold"

// "aauaaaaada"
smallestEquivalentString "leetcode" "programs" "sourcecode"
