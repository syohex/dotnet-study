let number (cs: char list) : char list * int =
    let rec number' cs acc =
        match cs with
        | [] -> [], acc
        | h :: t when h >= '0' && h <= '9' -> number' t (acc * 10 + (int h - int '0'))
        | _ -> cs, acc

    number' cs 0

type ExprOp =
    | Plus
    | Minus

type TermOp =
    | Mul
    | Div

let rec expr (cs: char list) : char list * int =
    let rec expr' cs (op: Option<ExprOp>) acc =
        match cs, op with
        | [], _ -> [], acc
        | h :: t, _ when h = '+' -> expr' t (Some(Plus)) acc
        | h :: t, _ when h = '-' -> expr' t (Some(Minus)) acc
        | _, Some (o) ->
            let cs', value = term cs

            match o with
            | Plus -> expr' cs' None (acc + value)
            | Minus -> expr' cs' None (acc - value)
        | _, _ -> cs, acc

    let cs', value = term cs
    expr' cs' None value

and term (cs: char list) : char list * int =
    let rec term' cs (op: Option<TermOp>) acc =
        match cs, op with
        | [], _ -> [], acc
        | h :: t, _ when h = '*' -> term' t (Some(Mul)) acc
        | h :: t, _ when h = '/' -> term' t (Some(Div)) acc
        | _, Some (o) ->
            let cs', value = factor cs 0 false

            match o with
            | Mul -> term' cs' None (acc * value)
            | Div -> term' cs' None (acc / value)
        | _, _ -> cs, acc

    let cs', value = factor cs 0 false
    term' cs' None value

and factor (cs: char list) (acc: int) (isNegative: bool) : char list * int =
    match cs with
    | [] -> [], acc
    | h :: t when h = '-' -> factor t acc true
    | h :: t when h = '(' ->
        let cs', acc' = expr t
        (List.tail cs'), acc'
    | _ ->
        let cs', value = number cs
        cs', (if isNegative then -value else value)

let calc (s: string) : int =
    let cs = s |> Seq.filter (fun c -> c <> ' ') |> Seq.toList
    expr cs |> snd

// 123
calc "123"

// 3
calc "1 + 2"

// 6
calc "2 * 3"

// 9
calc "(5 + 4)"

// 8
calc "(1 + (5 + 4) - 2)"

// 5
calc "1+2*6/(10-7)"

// 23
calc "(1+(4+5+2)-3)+(6+8)"
