open System

let addBinary (a: string) (b: string) : string =
    let rec addBinary' ca cb carry (acc: char list) =
        match ca, cb with
        | [], [] ->
            if carry = 1 then
                String.Concat('1' :: acc)
            else
                String.Concat acc
        | h :: t, [] ->
            match h + carry with
            | 2 -> addBinary' t [] 1 ('0' :: acc)
            | 1 -> addBinary' t [] 0 ('1' :: acc)
            | 0 -> addBinary' t [] 0 ('0' :: acc)
            | _ -> failwith "never reach here"
        | [], h :: t ->
            match h + carry with
            | 2 -> addBinary' [] t 1 ('0' :: acc)
            | 1 -> addBinary' [] t 0 ('1' :: acc)
            | 0 -> addBinary' [] t 0 ('0' :: acc)
            | _ -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            match h1 + h2 + carry with
            | 3 -> addBinary' t1 t2 1 ('1' :: acc)
            | 2 -> addBinary' t1 t2 1 ('0' :: acc)
            | 1 -> addBinary' t1 t2 0 ('1' :: acc)
            | 0 -> addBinary' t1 t2 0 ('0' :: acc)
            | _ -> failwith "never reach here"

    let strToIntList = Seq.map (fun c -> int c - int '0') >> Seq.rev >> Seq.toList
    let ca, cb = strToIntList a, strToIntList b
    addBinary' ca cb 0 []

// "100"
addBinary "11" "1"

// "10101"
addBinary "1010" "1011"

// "10"
addBinary "1" "1"

// "11110"
addBinary "1111" "1111"
