let maxOperations (nums: int list) (k: int) : int =
    let rec maxOperations' nums k m ret =
        match nums with
        | [] -> ret
        | h :: t ->
            let diff = k - h

            match Map.tryFind diff m with
            | Some (v) when v >= 1 -> maxOperations' t k (Map.add diff (v - 1) m) (ret + 1)
            | _ ->
                match Map.tryFind h m with
                | None -> maxOperations' t k (Map.add h 1 m) ret
                | Some (w) -> maxOperations' t k (Map.add h (w + 1) m) ret

    maxOperations' nums k Map.empty 0

// 2
maxOperations [ 1; 2; 3; 4 ] 5

// 1
maxOperations [ 3; 1; 3; 4; 3 ] 6
