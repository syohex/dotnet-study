let countSeniors (details: string list) : int =
    details
    |> List.filter (fun detail -> detail.[11] >= '6' && detail.[12] >= '1')
    |> List.length

// 2
countSeniors [ "7868190130M7522"; "5303914400F9211"; "9273338290F4010" ]

// 0
countSeniors [ "1313579440F2036"; "2921522980M5644" ]
