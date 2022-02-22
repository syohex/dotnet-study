let titleToNumber (columnTitle: string) : int =
    columnTitle
    |> Seq.fold (fun acc c -> (acc * 26) + (int c - int 'A' + 1)) 0

// 1
titleToNumber "A"

// 28
titleToNumber "AB"

// 701
titleToNumber "ZY"