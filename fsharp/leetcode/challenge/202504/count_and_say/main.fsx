let toRunLength (s: string) : string =
    let rec toRunLength' cs count c (acc: (int * char) list) : string =
        match cs with
        | [] ->
            let acc = (count, c) :: acc

            acc
            |> List.rev
            |> List.fold (fun acc (count, c) -> sprintf "%s%d%c" acc count c) ""
        | h :: t ->
            if h = c then
                toRunLength' t (count + 1) c acc
            else
                toRunLength' t 1 h ((count, c) :: acc)

    match Seq.toList s with
    | [] -> failwith "never reach here"
    | h :: t -> toRunLength' t 1 h []

let countAndSay (n: int) : string =
    let rec countAndSay' n s =
        if n = 0 then s else countAndSay' (n - 1) (toRunLength s)

    countAndSay' (n - 1) "1"

// "1211"
countAndSay 4

// "1"
countAndSay 1

countAndSay 20
