let isPowerOfFour (n: int) : bool =
    let countBit n = 
        let n = (n &&& 0x55555555) + ((n >>> 1 &&& 0x55555555))
        let n = (n &&& 0x33333333) + ((n >>> 2 &&& 0x33333333))
        let n = (n &&& 0x0F0F0F0F) + ((n >>> 4 &&& 0x0F0F0F0F))
        let n = (n &&& 0x00FF00FF) + ((n >>> 8 &&& 0x00FF00FF))
        let n = (n &&& 0x0000FFFF) + ((n >>> 16 &&& 0x0000FFFF))
        n

    let bits = countBit n
    bits = 1 && (n &&& 0xAAAAAAAA) = 0

// false
isPowerOfFour 0

// true
isPowerOfFour 1

// true
isPowerOfFour 64

// false
isPowerOfFour 6

// false
isPowerOfFour 2

// false
isPowerOfFour 19
