type Trie =
    | Leaf
    | TrieNode of (Trie * Trie)

let toBits (bits: int) (n: int) : int list =
    let rec toBits' count n acc =
        if count = bits then
            acc
        else
            let m =
                if (n &&& (1 <<< count)) <> 0 then
                    1
                else
                    0

            toBits' (count + 1) n (m :: acc)

    toBits' 0 n []

let rec insertIntoTrie (t: Trie) (bits: int list) : Trie =
    match bits with
    | [] -> TrieNode(Leaf, Leaf)
    | b :: bs ->
        match t with
        | Leaf ->
            if b = 0 then
                TrieNode(insertIntoTrie Leaf bs, Leaf)
            else
                TrieNode(Leaf, insertIntoTrie Leaf bs)
        | TrieNode (zero, one) ->
            if b = 0 then
                TrieNode(insertIntoTrie zero bs, one)
            else
                TrieNode(zero, insertIntoTrie one bs)

let maxXorValue (t: Trie) (bits: int list) : int =
    let rec maxXorValue' t bits acc =
        match bits with
        | [] -> acc
        | b :: bs ->
            if b = 0 then
                match t with
                | TrieNode (_, (TrieNode (_, _) as one)) -> maxXorValue' one bs ((acc * 2) + 1)
                | TrieNode ((TrieNode (_, _) as zero), Leaf) -> maxXorValue' zero bs (acc * 2)
                | TrieNode (Leaf, Leaf) -> maxXorValue' Leaf bs (acc * 2)
                | _ -> failwithf "never reach here: %A(b=%d, %A)" t b bs
            else
                match t with
                | TrieNode ((TrieNode (_, _) as zero), _) -> maxXorValue' zero bs ((acc * 2) + 1)
                | TrieNode (Leaf, (TrieNode (_, _) as one)) -> maxXorValue' one bs (acc * 2)
                | TrieNode (Leaf, Leaf) -> maxXorValue' Leaf bs (acc * 2)
                | _ -> failwithf "never reach here: %A(b=%d, %A)" t b bs

    maxXorValue' t bits 0

let findMaximumXOR (nums: int list) : int =
    let numBits = nums |> List.map (toBits 32)

    let trie =
        numBits
        |> List.fold (fun t n -> insertIntoTrie t n) Leaf

    numBits
    |> List.map (fun n -> maxXorValue trie n)
    |> List.max

// 28
findMaximumXOR [ 3; 10; 5; 25; 2; 8 ]

// 127
findMaximumXOR [ 14
                 70
                 53
                 83
                 49
                 91
                 36
                 80
                 92
                 51
                 66
                 70 ]
