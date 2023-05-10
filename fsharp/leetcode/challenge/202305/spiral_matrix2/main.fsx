type State =
    | Right
    | Down
    | Left
    | Up

let generateMatrix (n: int) : int[,] =
    let rec generateMatrix' i limit state row col (acc: int[,]) =
        if i > limit then
            acc
        else
            acc.[row, col] <- i
            let i' = i + 1

            match state with
            | Right ->
                if col + 1 < n && acc.[row, col + 1] = 0 then
                    generateMatrix' i' limit state row (col + 1) acc
                else
                    generateMatrix' i' limit Down (row + 1) col acc
            | Down ->
                if row + 1 < n && acc.[row + 1, col] = 0 then
                    generateMatrix' i' limit state (row + 1) col acc
                else
                    generateMatrix' i' limit Left row (col - 1) acc
            | Left ->
                if col - 1 >= 0 && acc.[row, col - 1] = 0 then
                    generateMatrix' i' limit state row (col - 1) acc
                else
                    generateMatrix' i' limit Up (row - 1) col acc
            | Up ->
                if row - 1 >= 0 && acc.[row - 1, col] = 0 then
                    generateMatrix' i' limit state (row - 1) col acc
                else
                    generateMatrix' i' limit Right row (col + 1) acc

    let acc = Array2D.zeroCreate n n
    generateMatrix' 1 (n * n) Right 0 0 acc

// [[1,2,3],[8,9,4],[7,6,5]]
generateMatrix 3

// [[1]]
generateMatrix 1
