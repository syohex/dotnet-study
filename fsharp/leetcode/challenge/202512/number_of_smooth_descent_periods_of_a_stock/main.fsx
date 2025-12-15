let getDescentPeriods (prices: int list) : int64 =
    let len = List.length prices

    let countCombinations left right =
        let count = right - left
        (1 + count) * count / 2

    let rec getDescentPeriods' prices prev left acc =
        match prices with
        | [] -> acc + (int64 <| countCombinations left len)
        | (i, h) :: t ->
            if h = prev - 1 then
                getDescentPeriods' t h left acc
            else
                let acc = acc + (int64 <| countCombinations left i)
                getDescentPeriods' t h i acc

    let prev = List.head prices
    let prices = prices |> List.indexed |> List.skip 1
    getDescentPeriods' prices prev 0 0L

// 7
getDescentPeriods [ 3; 2; 1; 4 ]

// 4
getDescentPeriods [ 8; 6; 7; 7 ]

// 1
getDescentPeriods [ 1 ]

// 10
getDescentPeriods [ 4; 3; 2; 1 ]
