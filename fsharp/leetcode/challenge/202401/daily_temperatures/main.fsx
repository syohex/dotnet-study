let dailyTemperatures (temperatures: int[]) : int[] =
    let rec popLowerTemperatures i stack (acc: int[]) =
        match stack with
        | [] -> acc, []
        | h :: t ->
            if temperatures.[h] < temperatures.[i] then
                acc.[h] <- i - h
                popLowerTemperatures i t acc
            else
                acc, stack

    let rec dailyTemperatures' i stack acc =
        if i >= Array.length temperatures then
            acc
        else
            let acc', stack' = popLowerTemperatures i stack acc
            dailyTemperatures' (i + 1) (i :: stack') acc'

    let acc = Array.zeroCreate temperatures.Length
    dailyTemperatures' 0 [] acc

// [1,1,4,2,1,1,0,0]
dailyTemperatures [| 73; 74; 75; 71; 69; 72; 76; 73 |]

// [1,1,1,0]
dailyTemperatures [| 30; 40; 50; 60 |]

// [1,1,0]
dailyTemperatures [| 30; 60; 90 |]
