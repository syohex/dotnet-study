let maxBottlesDrink (numBottles: int) (numExchange: int) : int =
    let rec maxBottlesDrink' empties numExchange acc =
        if empties < numExchange then
            acc
        else
            let empties = empties - numExchange + 1
            maxBottlesDrink' empties (numExchange + 1) (acc + 1)

    maxBottlesDrink' numBottles numExchange numBottles

// 15
maxBottlesDrink 13 6

// 13
maxBottlesDrink 10 3
