let removeStones (stones: (int * int) list) : int =
    let len = List.length stones

    let toGraph (stones: (int * int)[]) =
        let acc = Array.init len (fun _ -> [])

        for i in 0 .. (len - 1) do
            for j in (i + 1) .. (len - 1) do
                if fst stones.[i] = fst stones.[j] || snd stones.[i] = snd stones.[j] then
                    acc.[i] <- j :: acc.[i]
                    acc.[j] <- i :: acc.[j]

        acc

    let rec markCycleGraph q (graph: int list[]) visited =
        match q with
        | [] -> visited
        | h :: t ->
            let visited = Set.add h visited
            let nexts = graph.[h] |> List.filter (fun n -> not <| Set.contains n visited)
            markCycleGraph (t @ nexts) graph visited

    let rec countCycleGraph i graph visited acc =
        if i >= len then
            acc
        elif Set.contains i visited then
            countCycleGraph (i + 1) graph visited acc
        else
            let visited = markCycleGraph [ i ] graph visited
            countCycleGraph (i + 1) graph visited (acc + 1)

    let stones = List.toArray stones
    let graph = toGraph stones
    let cycleGraphs = countCycleGraph 0 graph Set.empty 0

    len - cycleGraphs

// 5
removeStones [ (0, 0); (0, 1); (1, 0); (1, 2); (2, 1); (2, 2) ]

// 3
removeStones [ (0, 0); (0, 2); (1, 1); (2, 0); (2, 2) ]

// 0
removeStones [ (0, 0) ]
