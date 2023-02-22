let canShip (weights: int list) (capacity: int) (days: int) : bool =
    let rec canShip' totalWeight totalDays weights capacity days =
        match weights with
        | [] -> totalDays <= days
        | h :: t ->
            let totalWeight' = totalWeight + h

            if totalWeight' > capacity then
                canShip' h (totalDays + 1) t capacity days
            else
                canShip' totalWeight' totalDays t capacity days

    canShip' 0 1 weights capacity days

let shipWithinDays (weights: int list) (days: int) : int =
    let rec shipWithinDays' left right =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if canShip weights mid days then
                shipWithinDays' left mid
            else
                shipWithinDays' (mid + 1) right

    let maxWeight = List.max weights
    let totalWeight = List.sum weights

    shipWithinDays' maxWeight totalWeight

// 15
shipWithinDays [ 1..10 ] 5

// 6
shipWithinDays [ 3; 2; 2; 4; 1; 4 ] 3

// 3
shipWithinDays [ 1; 2; 3; 1; 1 ] 4
