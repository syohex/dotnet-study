let canCompleteCircuit (gas: int list) (cost: int list) : int =
    let rec canCompleteCircuit' gas cost pos total current =
        match (gas, cost) with
        | (h1 :: t1, h2 :: t2) ->
            let newTotal = total + h1 - h2
            let newCurrent = current + h1 - h2

            if newCurrent >= 0 then
                canCompleteCircuit' t1 t2 pos newTotal newCurrent
            else
                canCompleteCircuit' t1 t2 (pos + 1) newTotal 0
        | _ -> if total < 0 then -1 else pos

    canCompleteCircuit' gas cost 0 0 0

canCompleteCircuit [ 1 .. 5 ] [
    3
    4
    5
    1
    2
]

canCompleteCircuit [ 2; 3; 4 ] [
    3
    4
    3
]
