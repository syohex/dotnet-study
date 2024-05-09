let maximumHappinessSum (happiness: int list) (k: int) : int64 =
    let rec maximumHappinessSum' (happiness: int64 list) k diff ret =
        match happiness with
        | [] -> ret
        | h :: t ->
            let v = h - diff

            if v <= 0 || k <= 0 then
                ret
            else
                maximumHappinessSum' t (k - 1) (diff + 1L) (ret + v)

    let happiness' = happiness |> List.sort |> List.rev |> List.map int64
    maximumHappinessSum' happiness' k 0L 0L

// 4
maximumHappinessSum [ 1; 2; 3 ] 2

// 1
maximumHappinessSum [ 1; 1; 1; 1 ] 2

// 5
maximumHappinessSum [ 2; 3; 4; 5 ] 1
