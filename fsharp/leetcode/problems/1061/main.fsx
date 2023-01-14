open System

let createGraph (s1: string) (s2: string) : Map<char, char list> =
    let rec createGraph' cs1 cs2 acc =
        match cs1, cs2 with
        | [], [] -> acc
        | h1 :: t1, h2 :: t2 ->
            let acc' =
                match Map.tryFind h1 acc with
                | Some (v) -> Map.add h1 (h2 :: v) acc
                | None -> Map.add h1 [ h2 ] acc

            let acc'' =
                match Map.tryFind h2 acc' with
                | Some (v) -> Map.add h2 (h1 :: v) acc'
                | None -> Map.add h2 [ h1 ] acc'

            createGraph' t1 t2 acc''
        | _, _ -> failwith "never reach here"

    createGraph' (s1 |> Seq.toList) (s2 |> Seq.toList) Map.empty

let findSmallestCharacter (c: char) (graph: Map<char, char list>) : char =
    let rec findSmallestCharacter' (c: char) graph visited =
        match Map.tryFind c graph with
        | None -> c, visited
        | Some (nexts) ->
            nexts
            |> List.filter (fun n -> Set.contains n visited |> not)
            |> List.fold
                (fun (acc, vs) n ->
                    let child, vs' = findSmallestCharacter' n graph (Set.add n vs)
                    Math.Min(int acc, int child) |> char, vs')
                (c, visited)

    findSmallestCharacter' c graph Set.empty |> fst


let smallestEquivalentString (s1: string) (s2: string) (baseStr: string) : string =
    let graph = createGraph s1 s2

    let table =
        seq { 'a' .. 'z' }
        |> Seq.fold
            (fun acc c ->
                let minChar = findSmallestCharacter c graph
                Map.add c minChar acc)
            Map.empty

    baseStr
    |> Seq.map (fun c -> Map.find c table)
    |> String.Concat

// "makkek"
smallestEquivalentString "parker" "morris" "parser"

// "hdld"
smallestEquivalentString "hello" "world" "hold"

// "aauaaaaada"
smallestEquivalentString "leetcode" "programs" "sourcecode"
