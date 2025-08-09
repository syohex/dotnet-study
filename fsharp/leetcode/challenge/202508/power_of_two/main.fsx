let isPowerOfTwo (n: int) : bool =
    if n <= 0 then
        false
    else
        let v = 1 <<< 31
        v % n = 0

// true
isPowerOfTwo 1

// true
isPowerOfTwo 16

// false
isPowerOfTwo 3

// false
isPowerOfTwo 0

// false
isPowerOfTwo -2
