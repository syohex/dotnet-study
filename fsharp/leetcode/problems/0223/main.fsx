let computeArea ((ax1, ay1, ax2, ay2): (int * int * int * int)) (bx1, by1, bx2, by2) =
    let area1 = (ax2 - ax1) * (ay2 - ay1)
    let area2 = (bx2 - bx1) * (by2 - by1)

    let overwrapWidth =
        System.Math.Min(ax2, bx2)
        - System.Math.Max(ax1, bx1)

    let overwrapHeight =
        System.Math.Min(ay2, by2)
        - System.Math.Max(ay1, by1)

    if overwrapWidth > 0 && overwrapHeight > 0 then
        area1 + area2 - (overwrapWidth * overwrapHeight)
    else
        area1 + area2


// 45
computeArea (-3, 0, 3, 4) (0, -1, 9, 2)

// 16
computeArea (-2, -2, 2, 2) (-2, -2, 2, 2)
