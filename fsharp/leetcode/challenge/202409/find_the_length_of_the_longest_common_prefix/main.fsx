type Trie = Trie of Map<int, Trie>

let emptyTrie = Trie(Map.empty)

let rec trieInsert (ns: int list) (Trie m: Trie) : Trie =
    match ns with
    | [] -> Trie(m)
    | h :: t ->
        match Map.tryFind h m with
        | Some(v) -> Trie(Map.add h (trieInsert t v) m)
        | None -> Trie(Map.add h (trieInsert t (Trie(Map.empty))) m)

let findLongestLength (ns: int list) (t: Trie) : int =
    let rec findLongestLength' ns (Trie m) acc =
        match ns with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h m with
            | Some(v) -> findLongestLength' t v (acc + 1)
            | None -> acc

    findLongestLength' ns t 0

let toDigit (n: int) : int list =
    let rec toDigit' n acc =
        if n = 0 then acc else toDigit' (n / 10) ((n % 10) :: acc)

    if n = 0 then [ 0 ] else toDigit' n []

let longestCommonPrefix (arr1: int list) (arr2: int list) : int =
    let trie =
        arr1
        |> List.map toDigit
        |> List.fold (fun acc ns -> trieInsert ns acc) emptyTrie

    arr2
    |> List.map toDigit
    |> List.fold (fun acc ns -> max acc (findLongestLength ns trie)) 0

// 3
longestCommonPrefix [ 1; 10; 100 ] [ 1000 ]

// 0
longestCommonPrefix [ 1; 2; 3 ] [ 4; 4; 4 ]
