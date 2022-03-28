let findingUsersActiveMinutes (logs: int list list) (k: int) : int list =
    let rec toFreq logs m =
        match logs with
        | [] -> m
        | [ id; time ] :: tail ->
            match Map.tryFind id m with
            | None -> toFreq tail (Map.add id (Set.empty |> Set.add time) m)
            | Some (s) -> toFreq tail (Map.add id (s |> Set.add time) m)
        | _ -> failwith "never reach here"

    toFreq logs Map.empty
    |> Map.values
    |> Seq.map (fun s -> Set.count s)
    |> Seq.fold
        (fun (acc: int []) len ->
            acc.[len - 1] <- acc.[len - 1] + 1
            acc)
        (Array.zeroCreate k)
    |> Array.toList


// [0,2,0,0,0]
findingUsersActiveMinutes
    [ [ 0; 5 ]
      [ 1; 2 ]
      [ 0; 2 ]
      [ 0; 5 ]
      [ 1; 3 ] ]
    5

// [1,1,0,0]
findingUsersActiveMinutes [ [ 1; 1 ]; [ 2; 2 ]; [ 2; 3 ] ] 4
