let isPossible (nums: int list) : bool =
    let rec isPossible' nums freq seqs =
        match nums with
        | [] -> true
        | h :: t ->
            let count = Map.find h freq

            if count = 0 then
                isPossible' t freq seqs
            else
                let freq' = Map.add h (count - 1) freq

                match Map.tryFind (h - 1) seqs with
                | Some (v) when v > 0 ->
                    let seqs' = seqs |> Map.add (h - 1) (v - 1)

                    match Map.tryFind h seqs with
                    | Some (x) -> isPossible' t freq' (Map.add h (x + 1) seqs')
                    | None -> isPossible' t freq' (Map.add h 1 seqs')
                | _ ->
                    match Map.tryFind (h + 1) freq', Map.tryFind (h + 2) freq' with
                    | Some (n1), Some (n2) when n1 > 0 && n2 > 0 ->
                        let freq'' =
                            freq'
                            |> Map.add (h + 1) (n1 - 1)
                            |> Map.add (h + 2) (n2 - 1)

                        match Map.tryFind (h + 2) seqs with
                        | Some (x) -> isPossible' t freq'' (Map.add (h + 2) (x + 1) seqs)
                        | None -> isPossible' t freq'' (Map.add (h + 2) 1 seqs)
                    | _, _ -> false

    let freq =
        nums
        |> List.fold
            (fun acc n ->
                match Map.tryFind n acc with
                | Some (v) -> Map.add n (v + 1) acc
                | None -> Map.add n 1 acc)
            Map.empty

    isPossible' nums freq Map.empty


// true
isPossible [ 1; 2; 3; 4; 5; 6 ]
// true
isPossible [ 1; 2; 3; 3; 4; 5 ]
// true
isPossible [ 1; 2; 3; 3; 4; 4; 5; 5 ]
// false
isPossible [ 1; 2; 3; 4; 4; 5 ]
