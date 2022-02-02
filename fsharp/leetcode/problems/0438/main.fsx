let equalArray a b =
    (a, b)
    ||> Array.zip
    |> Array.forall (fun (x, y) -> x = y)

let slices (count: int) (a: char []) =
    let rec slices' count a n acc =
        if n + count > (Array.length a) then
            acc |> List.rev
        else
            slices' count a (n + 1) (a.[n..n + count - 1] :: acc)

    slices' count a 0 []

let sliceToTable (cs: char []) : int [] =
    cs
    |> Array.map (fun c -> int c - int 'a')
    |> Array.fold
        (fun table i ->
            table.[i] <- table.[i] + 1
            table)
        (Array.zeroCreate 26)

let findAnagrams (s: string) (p: string) : int list =
    if (Seq.length s) < (Seq.length p)
    then
        []
    else
        let pTable = p |> Seq.toArray |> sliceToTable

        let sTables =
            s
            |> Seq.toArray
            |> slices (Seq.length p)
            |> List.map sliceToTable

        sTables
        |> List.mapi (fun i t -> (i, t))
        |> List.filter (fun (_, t) -> equalArray t pTable)
        |> List.map (fun (i, _) -> i)

// [0, 6]
findAnagrams "cbaebabacd" "abc"

// [0, 1, 2]
findAnagrams "abab" "ab"

// []
findAnagrams "abc" "abcd"