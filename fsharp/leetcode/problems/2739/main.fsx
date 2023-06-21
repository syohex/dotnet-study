let distanceTraveled (mainTank: int) (additionalTank: int) : int =
    let rec distanceTraveled' mainTank additionalTank acc =
        if mainTank < 5 then
            acc + mainTank * 10
        else
            let mainTank' = mainTank - 5
            let acc' = acc + 50

            if additionalTank > 0 then
                distanceTraveled' (mainTank' + 1) (additionalTank - 1) acc'
            else
                distanceTraveled' mainTank' additionalTank acc'

    distanceTraveled' mainTank additionalTank 0

// 60
distanceTraveled 5 10

// 10
distanceTraveled 1 2

// 110
distanceTraveled 10 1
