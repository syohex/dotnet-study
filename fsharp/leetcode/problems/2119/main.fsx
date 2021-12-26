let isSameAfterReversals (num: int) : bool =
    let rec f n lst =
        match n with
        | 0 -> lst
        | m -> f (m / 10) ((m % 10) :: lst)

    let ret = f num []

    match List.rev ret with
    | [] -> true
    | _ :: [] -> true
    | head :: _ -> head <> 0

let check (e: bool) : unit =
    if not e then
        raise (System.Exception "unexpected")

check (isSameAfterReversals 555)
check (not (isSameAfterReversals 180))
check (isSameAfterReversals 0)
