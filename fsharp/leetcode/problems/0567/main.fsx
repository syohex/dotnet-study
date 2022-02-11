let splitString (s: string) (len: int) : char list list =
    let rec splitString' cs len acc =
        if (List.length cs) < len then
            acc |> List.rev
        else
            let v = cs |> List.take len
            splitString' (List.tail cs) len (v :: acc)

    let cs = s |> Seq.toList
    splitString' cs len []

let strToIndexes (s: string) : int list =
    s
    |> Seq.toList
    |> List.map (fun c -> (int c) - (int 'a'))

let charListToIndexes (cs: char list) : int list =
    cs |> List.map (fun c -> (int c) - (int 'a'))

let toFreqTable (indexes: int list) : (int []) =
    let rec toFreqTable' indexes (acc: int []) =
        match indexes with
        | [] -> acc
        | head :: tail ->
            acc.[head] <- acc.[head] + 1
            toFreqTable' tail acc

    toFreqTable' indexes (Array.init 26 (fun _ -> 0))

let equalFreqTable (a: int []) (b: int []) (len: int) : bool =
    let rec equalFreqTable' (a: int []) (b: int []) i len =
        if i > len then true
        else if a.[i] <> b.[i] then false
        else equalFreqTable' a b (i + 1) len

    equalFreqTable' a b 0 len

let checkInclusion (s1: string) (s2: string) : bool =
    let s1Len = Seq.length s1
    let s1Table = strToIndexes s1 |> toFreqTable

    let s2Tables =
        splitString s2 s1Len
        |> List.map (charListToIndexes >> toFreqTable)

    s2Tables
    |> List.tryFind (fun t -> equalFreqTable s1Table t s1Len)
    |> Option.isSome

// true
checkInclusion "ab" "eidbaooo"

// false
checkInclusion "ab" "eidboaooo"

// false
checkInclusion "abc" "ab"