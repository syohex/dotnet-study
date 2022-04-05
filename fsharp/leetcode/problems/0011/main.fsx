let maxArea (height: int list) : int =
    let rec maxArea' (height: int []) left right ret =
        if left >= right then
            ret
        else
            let h = System.Math.Min(height.[left], height.[right])
            let w = right - left
            let ret' = System.Math.Max(ret, h * w)

            if height.[left] < height.[right] then
                maxArea' height (left + 1) right ret'
            elif height.[left] > height.[right] then
                maxArea' height left (right - 1) ret'
            else
                maxArea' height (left + 1) (right - 1) ret'

    let height' = height |> List.toArray
    maxArea' height' 0 (height'.Length - 1) System.Int32.MinValue

// 49
maxArea [ 1; 8; 6; 2; 5; 4; 8; 3; 7 ]

// 1
maxArea [ 1; 1 ]
