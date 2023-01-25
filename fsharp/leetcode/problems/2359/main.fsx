let closestMeetingNode (edges: int[]) (node1: int) (node2: int) : int =
    let rec getDistance node dist (acc: int[]) =
        acc.[node] <- dist
        let next = edges.[node]

        if next = -1 || acc.[next] <> -1 then
            acc
        else
            getDistance next (dist + 1) acc

    let len = edges.Length
    let d1 = getDistance node1 0 (Array.init len (fun _ -> -1))
    let d2 = getDistance node2 0 (Array.init len (fun _ -> -1))

    d1
    |> Array.zip d2
    |> Array.indexed
    |> Array.fold
        (fun (closest, minDist) (i, (dist1, dist2)) ->
            if dist1 <> -1 && dist2 <> -1 then
                let d = System.Math.Max(dist1, dist2)
                if d < minDist then i, d else closest, minDist
            else
                closest, minDist)
        (-1, System.Int32.MaxValue)
    |> fst

// 2
closestMeetingNode [| 2; 2; 3; -1 |] 0 1

// 2
closestMeetingNode [| 1; 2; -1 |] 0 2

// -1
closestMeetingNode [| -1; -1; -1 |] 0 2
