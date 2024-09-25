type Trie = Trie of Map<char, (int * Trie)>

let emptyTrie = Trie(Map.empty)

let trieInsert (s: string) (t: Trie) : Trie =
    let rec trieInsert' cs (Trie m) =
        match cs with
        | [] -> Trie(m)
        | h :: t ->
            match Map.tryFind h m with
            | Some((score, trie)) -> Trie(Map.add h (score + 1, trieInsert' t trie) m)
            | None -> Trie(Map.add h (1, trieInsert' t emptyTrie) m)

    trieInsert' (Seq.toList s) t

let trieCountScore (s: string) (t: Trie) : int =
    let rec trieCountScore' cs (Trie m) acc =
        match cs with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h m with
            | None -> acc
            | Some((score, trie)) -> trieCountScore' t trie (score + acc)

    trieCountScore' (Seq.toList s) t 0

let sumPrefixScores (words: string list) : int list =
    let trie = words |> List.fold (fun acc word -> trieInsert word acc) emptyTrie
    words |> List.map (fun word -> trieCountScore word trie)

// [5,4,3,2]
sumPrefixScores [ "abc"; "ab"; "bc"; "b" ]

// [4]
sumPrefixScores [ "abcd" ]
