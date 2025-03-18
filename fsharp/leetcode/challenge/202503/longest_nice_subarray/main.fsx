let longestNiceSubArray (nums: int[]) : int =
    let rec shrinkWindow left num bits =
        if bits &&& num = 0 then
            left, bits
        else
            shrinkWindow (left + 1) num (bits ^^^ nums.[left])

    let rec longestNiceSubArray' left right bits acc =
        if right >= nums.Length then
            acc
        else
            let num = nums.[right]
            let left, bits = shrinkWindow left num bits
            longestNiceSubArray' left (right + 1) (bits ||| num) (max acc (right - left + 1))

    longestNiceSubArray' 0 0 0 1

// 3
longestNiceSubArray [| 1; 3; 8; 48; 10 |]

// 1
longestNiceSubArray [| 3; 1; 5; 11; 13 |]
