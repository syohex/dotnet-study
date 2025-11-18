let isOneBitCharacter (bits: int list) : bool =
    let rec isOneBitCharacter' bits =
        match bits with
        | [] -> false
        | 0 :: [] -> true
        | _ :: [] -> false
        | 0 :: t -> isOneBitCharacter' t
        | 1 :: 0 :: t
        | 1 :: 1 :: t -> isOneBitCharacter' t
        | _ -> failwith "never reach here"

    isOneBitCharacter' bits

// true
isOneBitCharacter [ 1; 0; 0 ]

// false
isOneBitCharacter [ 1; 1; 1; 0 ]
