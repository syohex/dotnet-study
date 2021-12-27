open System.Collections.Generic

let is_palindrome (str: string) : bool =
    let rec inner (index: int) (orig: char array) (rev: char array) : bool =
        if index = orig.Length then
            true
        elif orig[ index ] = rev[ index ] then
            inner (index + 1) orig rev
        else
            false

    let reversed = str.ToCharArray() |> Array.rev
    inner 0 (str.ToCharArray()) reversed

let first_palindrome (words: string list) : string =
    try
        words |> List.find is_palindrome
    with
    | :? KeyNotFoundException -> ""
    | _ -> failwith "never reach here"

first_palindrome [ "abc"
                   "car"
                   "ada"
                   "racecar"
                   "cool" ]
