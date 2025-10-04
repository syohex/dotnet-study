let maxArea (height: int[]) : int =
    let rec maxArea' left right acc =
        if left >= right then
            acc
        else
            let width = right - left
            let acc = max acc ((min height.[left] height.[right]) * width)

            if height.[left] < height.[right] then
                maxArea' (left + 1) right acc
            else
                maxArea' left (right - 1) acc

    maxArea' 0 (height.Length - 1) 0

// 49
maxArea [| 1; 8; 6; 2; 5; 4; 8; 3; 7 |]

// 1
maxArea [| 1; 1 |]
