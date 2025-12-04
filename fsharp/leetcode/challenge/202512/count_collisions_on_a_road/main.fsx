let countCollisions (directions: string) : int =
    let rec countCollisions' directions count acc =
        match directions with
        | [] -> acc
        | h :: t ->
            match h with
            | 'L' ->
                if count >= 0 then
                    countCollisions' t 0 (acc + count + 1)
                else
                    countCollisions' t count acc
            | 'R' ->
                if count < 0 then
                    countCollisions' t 1 acc
                else
                    countCollisions' t (count + 1) acc
            | _ ->
                if count >= 1 then
                    countCollisions' t 0 (acc + count)
                else
                    countCollisions' t 0 acc

    countCollisions' (List.ofSeq directions) -1 0

// 5
countCollisions "RLRSLL"

// 0
countCollisions "LLRR"
