open System

let decodeAtIndex (s: string) (k: int) : string =
    let rec totalLength (cs: char list) (acc: int64) : int64 =
        match cs with
        | [] -> acc
        | h :: t ->
            if Char.IsAsciiDigit(h) then
                let n = int64 h - int64 '0'
                totalLength t (acc * n)
            else
                totalLength t (acc + 1L)

    let rec decodeAtIndex' (cs: char list) k (length: int64) =
        match cs with
        | [] -> failwith "never reach here"
        | h :: t ->
            let k' = k % length

            if k' = 0 && not <| Char.IsAsciiDigit(h) then
                string h
            elif Char.IsAsciiDigit(h) then
                let n = int64 h - int64 '0'
                decodeAtIndex' t k' (length / n)
            else
                decodeAtIndex' t k' (length - 1L)

    let cs = Seq.toList s
    let length = totalLength cs 0

    decodeAtIndex' (List.rev cs) k length

// "o"
decodeAtIndex "leet2code3" 10

// "h"
decodeAtIndex "ha22" 5

// "a"
decodeAtIndex "a2345678999999999999999" 1

// "a"
decodeAtIndex "a23" 6
