let bestClosingTime (customers: string) : int =
    let rec bestClosingTime' customers penalty minPenalty minHour =
        match customers with
        | [] -> minHour
        | (i, h) :: t ->
            let penalty = if h = 'Y' then penalty - 1 else penalty + 1

            let minPenalty, minHour =
                if penalty < minPenalty then
                    penalty, i + 1
                else
                    minPenalty, minHour

            bestClosingTime' t penalty minPenalty minHour

    let customers = Seq.toList customers

    let penalty =
        customers |> List.fold (fun acc c -> if c = 'Y' then acc + 1 else acc) 0

    bestClosingTime' (List.indexed customers) penalty penalty 0

// 2
bestClosingTime "YYNY"

// 0
bestClosingTime "NNNNN"

// 4
bestClosingTime "YYYY"
