let maximumEnergy (energy: int list) (k: int) : int =
    let rec maximumEnergy' i (dp: int[]) (energy: int[]) acc =
        if i < 0 then
            acc
        else
            if i + k >= energy.Length then
                dp.[i] <- energy.[i]
            else
                dp.[i] <- dp.[i + k] + energy.[i]

            maximumEnergy' (i - 1) dp energy (max acc dp.[i])

    let energy = Array.ofList energy
    let len = energy.Length
    let dp = Array.zeroCreate len
    maximumEnergy' (len - 1) dp energy System.Int32.MinValue

// 3
maximumEnergy [ 5; 2; -1; -5; 1 ] 3

// -1
maximumEnergy [ -2; -3; -1 ] 2
