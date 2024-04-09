let timeRequiredToBuy (tickets: int list) (k: int) : int =
    let rec timeRequiredToBuy' (tickets: int[]) i k ret =
        if i >= tickets.Length then
            timeRequiredToBuy' tickets 0 k ret
        elif tickets.[i] = 0 then
            timeRequiredToBuy' tickets (i + 1) k ret
        else
            tickets.[i] <- tickets.[i] - 1
            let ret' = ret + 1

            if i = k && tickets.[i] = 0 then
                ret'
            else
                timeRequiredToBuy' tickets (i + 1) k ret'

    timeRequiredToBuy' (List.toArray tickets) 0 k 0

// 6
timeRequiredToBuy [ 2; 3; 2 ] 2

// 8
timeRequiredToBuy [ 5; 1; 1; 1 ] 0
