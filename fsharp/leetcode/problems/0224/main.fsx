let rec number (cs: char list) (acc: int64) : char list * int64 =
    match cs with
    | [] -> [], acc
    | h :: t ->
        if int h >= int '0' && int h <= int '9' then
            let acc' = acc * 10L + ((int h - int '0') |> int64)
            number t acc'
        else
            cs, acc

let rec expr (cs: char list) (acc: int64) : char list * int64 =
    match cs with
    | [] -> [], acc
    | _ ->
        let cs', value = factor cs 0 false

        match cs' with
        | [] -> [], value
        | h :: t ->
            if h = '+' || h = '-' then
                let cs'', value2 = factor t 0 false

                if h = '+' then
                    expr cs'' (acc + value + value2)
                else
                    expr cs'' (acc + value - value2)
            else
                cs', acc + value

and factor (cs: char list) (acc: int64) (isNegative: bool) : char list * int64 =
    match cs with
    | [] -> [], acc
    | h :: t ->
        if h = '-' then
            factor t acc true
        elif h = '(' then
            let cs', value = expr t 0
            (List.tail cs'), value
        else
            let cs', value = number cs 0
            cs', (if isNegative then -value else value)

let calculate (s: string) : int =
    let cs =
        s |> Seq.filter (fun c -> c <> ' ') |> Seq.toList

    expr cs 0 |> snd |> int

// 2
calculate "1 + 1"

// 3
calculate "  2-1 + 2"

// 23
calculate "(1+(4+5+2)-3)+(6+8)"

// -12345
calculate "-12345"
