let pivotArray (nums: int list) (pivot: int) : int list =
    let pivotCount =
        nums |> List.filter ((=) pivot) |> List.length

    let lows = nums |> List.filter (fun n -> n < pivot)
    let highs = nums |> List.filter (fun n -> n > pivot)
    let pivots = List.init pivotCount (fun _ -> pivot)
    lows @ pivots @ highs

// [9,5,3,10,10,12,14]
pivotArray [ 9; 12; 5; 10; 14; 3; 10 ] 10

// [-3,2,4,3]
pivotArray [ -3; 4; 3; 2 ] 2
