let getNoZeroIntegers (n: int) : int * int =
    let isNoZeroInteger n = string n |> Seq.forall ((<>) '0')

    let rec getNoZeroIntegers' a =
        if a = n then
            failwith "never reach here"
        else
            let b = n - a

            if isNoZeroInteger a && isNoZeroInteger b then
                a, b
            else
                getNoZeroIntegers' (a + 1)

    getNoZeroIntegers' 1

// 1, 1
getNoZeroIntegers 2

// 2, 9
getNoZeroIntegers 11
