open System

let convertToTitle (columnNumber: int) : string =
    let rec convertToTitle' n (acc: char list) =
        if n = 0 then
            acc |> String.Concat
        else
            let n' = n - 1
            convertToTitle' (n' / 26) ((n' % 26 + int 'A' |> char) :: acc)

    convertToTitle' columnNumber []

// "A"
convertToTitle 1

// "AB"
convertToTitle 28

// "ZY"
convertToTitle 701
