let longestIdealString (s: string) (k: int) : int =
    let rec longestIdealString' cs dp =
        match cs with
        | [] -> Map.values dp |> Seq.max
        | h :: t ->
            let longest =
                seq { 'a' .. 'z' }
                |> Seq.fold
                    (fun acc c ->
                        match Map.tryFind c dp with
                        | None -> acc
                        | Some(v) -> if (abs ((int h) - (int c))) <= k then max acc v else acc)
                    0

            match Map.tryFind h dp with
            | None -> longestIdealString' t (Map.add h (longest + 1) dp)
            | Some(v) -> longestIdealString' t (Map.add h (max v (longest + 1)) dp)

    longestIdealString' (Seq.toList s) Map.empty

// 4
longestIdealString "acfgbd" 2

// 4
longestIdealString "abcd" 3
