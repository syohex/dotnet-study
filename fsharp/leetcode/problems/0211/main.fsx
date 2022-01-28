type Trie =
    | Leaf
    | TrieNode of bool * Map<char, Trie>

let addWord (word: string) (trie: Trie) : Trie =
    let rec addWord' cs trie =
        match cs with
        | [] ->
            match trie with
            | Leaf -> Leaf
            | TrieNode (_, table) -> TrieNode(true, table)
        | c :: tail ->
            match trie with
            | Leaf ->
                let node = addWord' tail Leaf
                TrieNode(false, (Map.empty |> Map.add c node))
            | TrieNode (isWord, table) ->
                match Map.tryFind c table with
                | None -> TrieNode(isWord, (table |> Map.add c (addWord' tail Leaf)))
                | Some (node) -> TrieNode(isWord, (table |> Map.add c (addWord' tail node)))

    let chars = word |> Seq.toList
    addWord' chars trie

let searchWord (word: string) (trie: Trie) : bool =
    let rec searchWord' cs trie =
        match cs with
        | [] ->
            match trie with
            | Leaf -> true
            | TrieNode (isWord, _) -> isWord
        | c :: tail ->
            if c = '.' then
                match trie with
                | Leaf -> false
                | TrieNode (_, table) -> Map.exists (fun _ n -> searchWord' tail n) table
            else
                match trie with
                | Leaf -> false
                | TrieNode (_, table) ->
                    match Map.tryFind c table with
                    | None -> false
                    | Some (t) -> searchWord' tail t


    let chars = word |> Seq.toList
    searchWord' chars trie

let trie =
    Leaf
    |> addWord "bad"
    |> addWord "dad"
    |> addWord "mad"

// false
searchWord "pad" trie

// true
searchWord "bad" trie

// true
searchWord ".ad" trie

// true
searchWord "b.." trie

// false
searchWord "...." trie
