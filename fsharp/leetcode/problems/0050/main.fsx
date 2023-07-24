let myPow (x: double) (n: int) : double =
    let rec myPow' x (n: int64) : double =
        match n with
        | 0L -> 1
        | 1L -> x
        | n when n < 0L -> 1.0 / myPow' x (-1L * n)
        | n when n % 2L = 1L ->
            let tmp = myPow' x ((n - 1L) / 2L)
            tmp * tmp * x
        | _ ->
            let tmp = myPow' x (n / 2L)
            tmp * tmp

    myPow' x (int64 n)

// 1024.0
myPow 2.0 10

// 9.26100
myPow 2.100 3

// 0.25
myPow 2.0 -2
