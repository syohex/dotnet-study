open System

let removeKDigits (num: string) (k: int) : string =
    let rec popBiggerNumber stack num k =
        if k = 0 then
            stack, 0
        else
            match stack with
            | [] -> [], k
            | h :: t -> if h > num then popBiggerNumber t num (k - 1) else stack, k

    let rec removeKDigits' cs k stack =
        match cs with
        | [] -> List.rev stack, k
        | h :: t ->
            let stack', k' = popBiggerNumber stack h k
            removeKDigits' t k' (h :: stack')

    let stack, k' = removeKDigits' (Seq.toList num) k []
    let stack = List.skip k' stack
    let stack = List.skipWhile ((=) '0') stack

    match stack with
    | [] -> "0"
    | _ -> String.Concat stack

// "1219"
removeKDigits "1432219" 3

// "200"
removeKDigits "10200" 1

// "10"
removeKDigits "10" 2
