let maximumUnits (boxTypes: (int * int) list) (truckSize: int) : int =
    let rec maximumUnits' boxTypes truckSize acc =
        if truckSize = 0 then
            acc
        else
            match boxTypes with
            | [] -> acc
            | (num, units) :: t ->
                let count = System.Math.Min(truckSize, num)
                maximumUnits' t (truckSize - count) (acc + (units * count))

    let boxTypes' =
        boxTypes
        |> List.sortWith (fun (_, unit1) (_, unit2) -> compare unit2 unit1)

    maximumUnits' boxTypes' truckSize 0

// 8
maximumUnits [ (1, 3); (2, 2); (3, 1) ] 4

// 91
maximumUnits [ (5, 10); (2, 5); (4, 7); (3, 9) ] 10
