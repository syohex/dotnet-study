let maximizeSquareHoleArea _n _m (hBars: int list) (vBars: int list) : int =
    let rec f bars prev curLen maxLen =
        match bars with
        | [] -> maxLen
        | h :: t ->
            let curLen = if h = prev + 1 then curLen + 1 else 1
            f t h curLen (max maxLen curLen)

    let hBars, vBars = List.sort hBars, List.sort vBars
    let maxH = f (List.tail hBars) (List.head hBars) 1 1
    let maxV = f (List.tail vBars) (List.head vBars) 1 1
    let minLen = min maxH maxV
    (minLen + 1) * (minLen + 1)

// 4
maximizeSquareHoleArea 2 1 [ 2; 3 ] [ 2 ]

// 4
maximizeSquareHoleArea 1 1 [ 2 ] [ 2 ]

// 4
maximizeSquareHoleArea 2 3 [ 2; 3 ] [ 2; 4 ]

// 9
maximizeSquareHoleArea 3 2 [ 3; 2; 4 ] [ 3; 2 ]
