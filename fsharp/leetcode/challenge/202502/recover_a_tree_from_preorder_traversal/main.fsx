type Tree =
    | Leaf
    | Node of int * Tree * Tree

let recoverFromPreorder (traversal: string) : Tree =
    let rec countDashes cs acc =
        match cs with
        | [] -> acc, []
        | h :: t -> if h = '-' then countDashes t (acc + 1) else acc, cs

    let rec readNumber cs acc =
        match cs with
        | [] -> acc, []
        | h :: t ->
            if System.Char.IsAsciiDigit(h) then
                readNumber t (acc * 10 + int h - int '0')
            else
                acc, cs

    let rec recoverFromPreorder' cs depth =
        let dashes, cs' = countDashes cs 0

        if dashes <> depth then
            Leaf, cs
        else
            let num, cs' = readNumber cs' 0
            let left, cs' = recoverFromPreorder' cs' (depth + 1)
            let right, cs' = recoverFromPreorder' cs' (depth + 1)
            Node(num, left, right), cs'

    recoverFromPreorder' (Seq.toList traversal) 0 |> fst

// [1,2,5,3,4,6,7]
recoverFromPreorder "1-2--3--4-5--6--7"

// [1,2,5,3,null,6,null,4,null,7]
recoverFromPreorder "1-2--3---4-5--6---7"

// [1,401,null,349,88,90]
recoverFromPreorder "1-401--349---90--88"
