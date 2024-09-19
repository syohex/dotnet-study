open System

let diffWaysToCompute (expression: string) : int list =
    let rec diffWaysToCompute' left right =
        if left = right then
            [ int expression.[left] - int '0' ]
        elif left + 1 = right && Char.IsDigit(expression.[left]) then
            [ int expression.[left] * 10 + int expression.[right] - int '0' ]
        else
            seq { left..right }
            |> Seq.fold
                (fun acc i ->
                    if Char.IsDigit(expression.[i]) then
                        acc
                    else
                        let lefts = diffWaysToCompute' left (i - 1)
                        let rights = diffWaysToCompute' (i + 1) right

                        let vals =
                            [ for v1 in lefts do
                                  for v2 in rights do
                                      match expression.[i] with
                                      | '+' -> yield v1 + v2
                                      | '-' -> yield v1 - v2
                                      | '*' -> yield v1 * v2
                                      | _ -> failwith "never reach here" ]

                        vals @ acc)
                []

    diffWaysToCompute' 0 (expression.Length - 1) |> List.sort

// [0,1]
diffWaysToCompute "2-1-1"

// [-34,-14,-10,-10,10]
diffWaysToCompute "2*3-4*5"
