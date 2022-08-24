let isPowerOfThree (n: int) : bool =
    let rec isPowerOfThree' (v: int64) (n: int64) (limit: int64)=
        if v > limit then
            false
        elif n = v then
            true
        else
            isPowerOfThree' (v * 3L) n limit

    let limit = System.Int32.MaxValue |> int64
    isPowerOfThree' 1L (n |> int64) limit

// false
isPowerOfThree 0

// true
isPowerOfThree 3

// true
isPowerOfThree 1

// true
isPowerOfThree 9

// false
isPowerOfThree 19682