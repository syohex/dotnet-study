type ParkingSystem =
    { cars: int[] }

    static member init (big: int) (medium: int) (small: int) : ParkingSystem =
        let cars = [| big; medium; small |]
        { cars = cars }

    static member addCar (carType: int) (p: ParkingSystem) : (bool * ParkingSystem) =
        let index = carType - 1

        if p.cars.[index] = 0 then
            false, p
        else
            p.cars.[index] <- p.cars.[index] - 1
            true, p

let p = ParkingSystem.init 1 1 0
let ret1, p1 = ParkingSystem.addCar 1 p
let ret2, p2 = ParkingSystem.addCar 2 p1
let ret3, p3 = ParkingSystem.addCar 3 p2
let ret4, _ = ParkingSystem.addCar 1 p3
printfn "%b %b %b %b" ret1 ret2 ret3 ret4
