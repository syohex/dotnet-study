let canBeValid (s: string) (locked: string) : bool =
    let rec f i s locked opens zeros =
        match s, locked with
        | [], [] -> Some((opens, zeros))
        | _, []
        | [], _ -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            if h2 = '0' then
                f (i + 1) t1 t2 opens (i :: zeros)
            elif h1 = '(' then
                f (i + 1) t1 t2 (i :: opens) zeros
            else
                match opens with
                | _ :: t -> f (i + 1) t1 t2 t zeros
                | _ ->
                    match zeros with
                    | _ :: t -> f (i + 1) t1 t2 opens t
                    | _ -> None

    let rec g opens zeros =
        match opens, zeros with
        | [], _ -> true
        | _, [] -> false
        | h1 :: t1, h2 :: t2 -> if h1 < h2 then g t1 t2 else false

    if s.Length % 2 <> 0 then
        false
    else
        match f 0 (Seq.toList s) (Seq.toList locked) [] [] with
        | None -> false
        | Some((opens, zeros)) -> g opens zeros

// true
canBeValid "))()))" "010100"

// true
canBeValid "()()" "0000"

// false
canBeValid ")" "0"

// true
canBeValid ")(" "00"

// true
canBeValid "()" "11"
