let parentToGraph (parent: int list) : Map<int, int list> =
    let rec parentToGraph' parent acc =
        match parent with
        | [] -> acc
        | (i, node) :: t ->
            match Map.tryFind node acc with
            | Some(v) -> parentToGraph' t (Map.add node (i :: v) acc)
            | None -> parentToGraph' t (Map.add node [ i ] acc)

    parentToGraph' (parent |> List.indexed |> List.tail) Map.empty

let longestPath (parent: int list) (s: string) : int =
    let rec longestPath' node graph (cs: char[]) acc =
        match Map.tryFind node graph with
        | None -> 1, acc
        | Some(children) ->
            let acc', first, second =
                children
                |> List.fold
                    (fun (acc, first, second) child ->
                        let childLongest, acc' = longestPath' child graph cs acc

                        if cs.[node] = cs.[child] then acc', first, second
                        else if childLongest > first then acc', childLongest, first
                        elif childLongest > second then acc', first, childLongest
                        else acc', first, second)
                    (acc, 0, 0)

            first + 1, System.Math.Max(acc', first + second + 1)

    let graph = parentToGraph parent
    longestPath' 0 graph (s |> Seq.toArray) 0 |> snd

// 3
longestPath [ -1; 0; 0; 1; 1; 2 ] "abacbe"

// 3
longestPath [ -1; 0; 0; 0 ] "aabc"
