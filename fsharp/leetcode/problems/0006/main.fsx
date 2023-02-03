open System

type State =
    | Down
    | Up

let rec convert'
    (cs: char list)
    (state: State)
    (row: int)
    (col: int)
    (numRows: int)
    (table: char[,])
    : (char[,] * int) =
    match cs with
    | [] -> table, row
    | h :: t ->
        table.[row, col] <- h

        match state with
        | Down ->
            if row + 1 = numRows then
                convert' t Up (row - 1) (col + 1) numRows table
            else
                convert' t Down (row + 1) col numRows table
        | Up ->
            if row = 0 then
                convert' t Down (row + 1) col numRows table
            else
                convert' t Up (row - 1) (col + 1) numRows table

let convert (s: string) (numRows: int) : string =
    if numRows = 1 then
        s
    else
        let table = Array2D.zeroCreate numRows s.Length
        let table', row = convert' (Seq.toList s) Down 0 0 numRows table

        table'
        |> Seq.cast<char>
        |> Seq.fold (fun acc c -> if int c <> 0 then c :: acc else acc) []
        |> Seq.rev
        |> String.Concat

// "PAHNAPLSIIGYIR"
convert "PAYPALISHIRING" 3

// "PINALSIGYAHRPI"
convert "PAYPALISHIRING" 4

// "A"
convert "A" 1

// "AB"
convert "AB" 1
