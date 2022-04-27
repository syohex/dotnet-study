let pairsToGraph (pairs: (int * int) list) : Map<int, int list> =
    let rec pairsToGraph' pairs acc =
        match pairs with
        | [] -> acc
        | (src, dest) :: t ->
            let acc' =
                match Map.tryFind src acc with
                | None -> Map.add src [ dest ] acc
                | Some (nodes) -> Map.add src (dest :: nodes) acc

            let acc'' =
                match Map.tryFind dest acc' with
                | None -> Map.add dest [ src ] acc'
                | Some (nodes) -> Map.add dest (src :: nodes) acc'

            pairsToGraph' t acc''

    pairsToGraph' pairs Map.empty

let collectReachableChars (s: string) (index: int) (graph: Map<int, int list>) (used: Set<int>) =
    let rec collectReachableChars' (s: string) index graph used chars indexes =
        let chars' = s.[index] :: chars
        let indexes' = index :: indexes
        let used' = Set.add index used

        match Map.tryFind index graph with
        | None -> chars', indexes', used'
        | Some (nodes) ->
            nodes
            |> List.fold
                (fun (chars, indexes, used) node ->
                    if Set.contains node used then
                        chars, indexes, used
                    else
                        collectReachableChars' s node graph used chars indexes)
                (chars', indexes', used')

    collectReachableChars' s index graph used [] []

let smallestStringWithSwaps (s: string) (pairs: (int * int) list) : string =
    let graph = pairsToGraph pairs
    let len = s.Length

    let sortedPair, _ =
        [ 0 .. len - 1 ]
        |> List.fold
            (fun (acc, used) index ->
                if Set.contains index used then
                    acc, used
                else
                    let chars, indexes, used' =
                        collectReachableChars s index graph used

                    (List.sort chars, List.sort indexes) :: acc, used')
            ([], Set.empty)

    let ca: char [] = Array.zeroCreate len

    sortedPair
    |> List.iter (fun (chars, indexes) ->
        List.zip chars indexes
        |> List.iter (fun (c, i) -> ca.[i] <- c))

    ca |> System.String

// "bacd"
smallestStringWithSwaps "dcab" [ (0, 3); (1, 2) ]

// "abcd"
smallestStringWithSwaps "dcab" [ (0, 3); (1, 2); (0, 2) ]

// "abc"
smallestStringWithSwaps "cba" [ (0, 1); (1, 2) ]

// "dcab"
smallestStringWithSwaps "dcab" []
