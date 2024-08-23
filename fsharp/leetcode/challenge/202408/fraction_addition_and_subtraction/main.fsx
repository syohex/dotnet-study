let parseSign (cs: char list) : (int * char list) =
    let rec parseSign' cs acc =
        match cs with
        | [] -> failwith "never reach here at parseSign"
        | h :: t ->
            match h with
            | '+' -> parseSign' t acc
            | '-' -> parseSign' t (-1 * acc)
            | _ -> acc, cs

    parseSign' cs 1

let parseNumber (cs: char list) : (int * char list) =
    let rec parseNumber' cs acc =
        match cs with
        | [] -> acc, []
        | h :: t ->
            match h with
            | '+'
            | '-' -> acc, cs
            | '/' -> acc, t
            | c ->
                let acc = 10 * acc + (int c - int '0')
                parseNumber' t acc

    let sign, cs = parseSign cs
    let num, cs = parseNumber' cs 0
    num * sign, cs

let parseFraction (cs: char list) : (int * int) list =
    let rec parseFraction' cs acc =
        match cs with
        | [] -> List.rev acc
        | _ ->
            let v1, cs = parseNumber cs
            let v2, cs = parseNumber cs
            parseFraction' cs ((v1, v2) :: acc)

    parseFraction' cs []

let rec gcd (a: int) (b: int) : int = if b = 0 then a else gcd b (a % b)

let fractionAddition (expression: string) : string =
    let cs = Seq.toList expression
    let fractions = parseFraction cs

    let (a, b) =
        fractions |> List.reduce (fun (a1, b1) (a2, b2) -> a1 * b2 + a2 * b1, b1 * b2)

    if a = 0 then
        "0/1"
    else
        let divisor = gcd (abs a) (abs b)
        sprintf "%d/%d" (a / divisor) (b / divisor)

// "0/1"
fractionAddition "-1/2+1/2"

// "1/3"
fractionAddition "-1/2+1/2+1/3"

// "-1/6"
fractionAddition "1/3-1/2"
