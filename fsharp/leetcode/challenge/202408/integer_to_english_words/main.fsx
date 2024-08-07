open System

let numberToWords (num: int) : string =
    let ones =
        [| "One"; "Two"; "Three"; "Four"; "Five"; "Six"; "Seven"; "Eight"; "Nine" |]

    let teens =
        [| "Ten"
           "Eleven"
           "Twelve"
           "Thirteen"
           "Fourteen"
           "Fifteen"
           "Sixteen"
           "Seventeen"
           "Eighteen"
           "Nineteen" |]

    let tens =
        [| "Twenty"
           "Thirty"
           "Fourty"
           "Fifty"
           "Sixty"
           "Seventy"
           "Eighty"
           "Ninety" |]

    let toHundred n acc =
        if n < 100 then
            acc, false
        else
            let m = n / 100
            "Hundred" :: ones.[m - 1] :: acc, true

    let toTens n acc =
        if n = 0 then acc, false
        elif n < 10 then ones.[n - 1] :: acc, true
        elif n < 20 then teens.[n - 10] :: acc, true
        else ones.[(n % 10) - 1] :: tens.[(n / 10) - 2] :: acc, true

    if num = 0 then
        "Zero"
    else
        [ (1_000_000_000, "Billion")
          (1_000_000, "Million")
          (1_000, "Thousand")
          (1, "") ]
        |> List.fold
            (fun (acc, num) (baseNum, unit) ->
                let num' = num / baseNum
                let acc, ret1 = toHundred num' acc
                let acc, ret2 = toTens (num' % 100) acc

                if (ret1 || ret2) && unit <> "" then
                    unit :: acc, num % baseNum
                else
                    acc, num % baseNum)
            ([], num)
        |> fst
        |> List.rev
        |> String.concat " "

// "One Hundred Twenty Three"
numberToWords 123

// "Twelve Thousand Three Hundred Forty Five"
numberToWords 12345

// "One Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven"
numberToWords 1234567

// "Zero"
numberToWords 0

// "Twenty One"
numberToWords 21

// "One Million"
numberToWords 1_000_000

// "One Million Ten"
numberToWords 1_000_010
