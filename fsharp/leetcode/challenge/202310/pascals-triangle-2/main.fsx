let getRow (rowIndex: int) : int[] =
    let rec getRow' i rowIndex (prev: int[]) =
        if i > rowIndex then
            prev
        else
            let cur =
                Array.init (i + 1) (fun j ->
                    if j = 0 then prev.[0]
                    elif j = i then prev.[j - 1]
                    else prev.[j - 1] + prev.[j])

            getRow' (i + 1) rowIndex cur

    getRow' 1 rowIndex [| 1 |]

// [1;3;3;1]
getRow 3

// [1]
getRow 0

// [1;1]
getRow 1

getRow 7
