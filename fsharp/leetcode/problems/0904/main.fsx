open System

let totalFruit (fruits: int list) : int =
    let rec totalFruit' (fruits: int[]) left right m (acc: int) =
        if right >= fruits.Length then
            acc
        else
            let m' =
                match Map.tryFind fruits.[right] m with
                | Some(v) -> Map.add fruits.[right] (v + 1) m
                | None -> Map.add fruits.[right] 1 m

            let m'', left' =
                if Map.count m' > 2 then
                    let lefts = Map.find fruits.[left] m'

                    if lefts = 1 then
                        Map.remove fruits.[left] m', left + 1
                    else
                        Map.add fruits.[left] (lefts - 1) m', left + 1
                else
                    m', left

            if Map.count m'' <= 2 then
                let total = Map.values m'' |> Seq.sum
                totalFruit' fruits left' (right + 1) m'' (Math.Max(acc, total))
            else
                totalFruit' fruits left' (right + 1) m'' acc

    totalFruit' (fruits |> List.toArray) 0 0 Map.empty 0

// 3
totalFruit [ 1; 2; 1 ]

// 3
totalFruit [ 0; 1; 2; 2 ]

// 4
totalFruit [ 1; 2; 3; 2; 2 ]

// 5
totalFruit [ 1; 0; 1; 4; 1; 4; 1; 2; 3 ]
