let averageWaitingTime (customers: (int * int) list) : double =
    let len = List.length customers

    let rec averageWaitingTime' customers finish totalWait =
        match customers with
        | [] -> double totalWait / double len
        | (arrival, wait) :: t ->
            let finish' = (max finish arrival) + wait
            let totalWait' = totalWait + (finish' - arrival)
            averageWaitingTime'  t finish' totalWait'

    averageWaitingTime' customers 0 0

// 5.0
averageWaitingTime [(1,2);(2,5);(4,3)]

// 3.25
averageWaitingTime [(5,2);(5,4);(10,3);(20,1)]
