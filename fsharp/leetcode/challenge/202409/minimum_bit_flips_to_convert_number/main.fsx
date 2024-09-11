let minBitFlips (start: int) (goal: int) : int =
    let b = start ^^^ goal
    let b = (b &&& 0x55555555) + ((b >>> 1) &&& 0x55555555)
    let b = (b &&& 0x33333333) + ((b >>> 2) &&& 0x33333333)
    let b = (b &&& 0x0f0f0f0f) + ((b >>> 4) &&& 0x0f0f0f0f)
    let b = (b &&& 0x00ff00ff) + ((b >>> 8) &&& 0x00ff00ff)
    (b &&& 0x0000ffff) + ((b >>> 16) &&& 0x0000ffff)

// 3
minBitFlips 10 7

// 3
minBitFlips 3 4
