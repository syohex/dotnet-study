let sortByBits (arr: int list) : int list =
    let countOnes (n: int) : int =
        let rec countOnes' n acc =
            if n = 0 then
                acc
            else
                let acc' = acc + if (n &&& 1) = 1 then 1 else 0
                countOnes' (n / 2) acc'

        countOnes' n 0

    let table =
        arr
        |> List.fold
            (fun acc n ->
                let ones = countOnes n
                Map.add n ones acc)
            Map.empty

    arr
    |> List.sortWith (fun a b ->
        let onesA = Map.find a table
        let onesB = Map.find b table

        if onesA = onesB then
            compare a b
        else
            compare onesA onesB)

// [0,1,2,4,8,3,5,6,7]
sortByBits [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]

// [1,2,4,8,16,32,64,128,256,512,1024]
sortByBits [ 1024
             512
             256
             128
             64
             32
             16
             8
             4
             2
             1 ]
