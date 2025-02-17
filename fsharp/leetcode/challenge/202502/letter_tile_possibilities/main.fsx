let numTilePossibilities (tiles: string) : int =
    let rec numTilePossibilities' (table: int[]) =
        seq { 0..25 }
        |> Seq.fold
            (fun acc i ->
                if table.[i] = 0 then
                    acc
                else
                    table.[i] <- table.[i] - 1
                    let acc = acc + 1 + numTilePossibilities' table
                    table.[i] <- table.[i] + 1
                    acc)
            0


    let table =
        tiles
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'A'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    numTilePossibilities' table

// 8
numTilePossibilities "AAB"

// 188
numTilePossibilities "AAABBC"

// 1
numTilePossibilities "V"
