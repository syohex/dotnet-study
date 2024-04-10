let deckRevealedIncreasing (deck: int list) : int[] =
    let rec deckRevealedIncreasing' deck q (ret: int[]) =
        match deck with
        | [] -> ret
        | h :: t ->
            match q with
            | [] -> failwith "never reach here"
            | i :: q' ->
                ret.[i] <- h

                match q' with
                | [] -> deckRevealedIncreasing' t [] ret
                | j :: q'' -> deckRevealedIncreasing' t (q'' @ [ j ]) ret

    let len = List.length deck
    let ret = Array.zeroCreate len
    let q = [ 0 .. (len - 1) ]
    deckRevealedIncreasing' (List.sort deck) q ret

// [2,13,3,11,5,17,7]
deckRevealedIncreasing [ 17; 13; 11; 2; 3; 5; 7 ]

// [1,1000]
deckRevealedIncreasing [ 1; 1000 ]
