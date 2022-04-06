let threeSumMulti (arr: int list) (target: int) : int =
    let rec threeSumMulti' arr target m ret =
        match arr with
        | [] -> ret
        | h :: t ->
            let ret' =
                t
                |> List.fold
                    (fun acc n ->
                        let diff = target - h - n

                        match Map.tryFind diff m with
                        | Some (v) -> (acc + v) % 1_000_000_007
                        | None -> acc)
                    ret

            let m' =
                match Map.tryFind h m with
                | Some (v) -> Map.add h (v + 1) m
                | None -> Map.add h 1 m

            threeSumMulti' t target m' ret'

    threeSumMulti' arr target Map.empty 0


// 20
threeSumMulti [ 1; 1; 2; 2; 3; 3; 4; 4; 5; 5 ] 8

// 12
threeSumMulti [ 1; 1; 2; 2; 2; 2 ] 5
