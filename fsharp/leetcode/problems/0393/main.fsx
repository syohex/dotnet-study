let validUtf8 (data: int list) : bool =
    let rec validUtf8' (data: uint8 []) pos =
        if pos >= data.Length then
            true
        elif (data.[pos] &&& 0b10000000uy) = 0b00000000uy then
            validUtf8' data (pos + 1)
        elif (data.[pos] &&& 0b11100000uy) = 0b11000000uy then
            if (pos + 1) >= data.Length then
                false
            elif (data.[pos + 1] &&& 0b11000000uy) = 0b10000000uy then
                validUtf8' data (pos + 2)
            else
                false
        elif (data.[pos] &&& 0b11110000uy) = 0b11100000uy then
            if (pos + 2) >= data.Length then
                false
            elif (data.[pos + 1] &&& 0b11000000uy) = 0b10000000uy
                 && (data.[pos + 2] &&& 0b11000000uy) = 0b10000000uy then
                validUtf8' data (pos + 3)
            else
                false
        elif (data.[pos] &&& 0b11111000uy) = 0b11110000uy then
            if (pos + 3) >= data.Length then
                false
            elif (data.[pos + 1] &&& 0b11000000uy) = 0b10000000uy
                 && (data.[pos + 2] &&& 0b11000000uy) = 0b10000000uy
                 && (data.[pos + 3] &&& 0b11000000uy) = 0b10000000uy then
                validUtf8' data (pos + 4)
            else
                false
        else
            false

    let data' = data |> List.map uint8 |> List.toArray
    validUtf8' data' 0

// true
validUtf8 [ 197; 130; 1 ]

// false
validUtf8 [ 235; 140; 4 ]
