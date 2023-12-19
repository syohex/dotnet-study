let imageSmoother (img: int[,]) : int[,] =
    let rows, cols = Array2D.length1 img, Array2D.length2 img

    let rec smooth x y row col count sum =
        if x >= 2 then
            sum / count
        elif y >= 2 then
            smooth (x + 1) -1 row col count sum
        else
            let row', col' = row + x, col + y

            if row' >= 0 && row' < rows && col' >= 0 && col' < cols then
                smooth x (y + 1) row col (count + 1) (sum + img.[row', col'])
            else
                smooth x (y + 1) row col count sum

    let rec imageSmoother' row col (acc: int[,]) =
        if row >= rows then
            acc
        elif col >= cols then
            imageSmoother' (row + 1) 0 acc
        else
            acc.[row, col] <- smooth -1 -1 row col 0 0
            imageSmoother' row (col + 1) acc

    imageSmoother' 0 0 (Array2D.zeroCreate rows cols)

let img1 = array2D [ [ 1; 1; 1 ]; [ 1; 0; 1 ]; [ 1; 1; 1 ] ]
// [[0,0,0],[0,0,0],[0,0,0]]
imageSmoother img1

let img2 = array2D [ [ 100; 200; 100 ]; [ 200; 50; 200 ]; [ 100; 200; 100 ] ]
// [[137,141,137],[141,138,141],[137,141,137]]
imageSmoother img2
