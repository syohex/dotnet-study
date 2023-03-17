type Trie =
    | Leaf
    | Node of Map<char, Trie> * bool

let trieInsert (word: string) (trie: Trie) =
    let rec trieInsert' cs trie =
        match cs with
        | [] -> Leaf
        | h :: t ->
            let table = match trie with
                        | Leaf -> Map.empty
                        | Node(m, _) -> m

            match Map.tryFind h table with
            | None ->
                let child = trieInsert' t Leaf
                let table' = Map.add h child table
                Node(table', child = Leaf)
            | Some(child) ->
                let child' = trieInsert' t child
                let table' = Map.add h child' table
                Node(table', child' = Leaf)

    trieInsert' (word |>Seq.toList) trie

let trieSearch (word: string) (trie: Trie) =
    let rec trieSearch' cs trie =
        match cs with
        | [] -> false
        | h :: t ->
            match trie with
            | Leaf -> false
            | Node(m, isLeaf) ->
                if isLeaf then
                    true
                else
                    match Map.tryFind h m with
                    | None -> false
                    | Some(child) ->
                        trieSearch' t child

    trieSearch' (word |>Seq.toList) trie

let trieStartsWith (word: string) (trie: Trie) =
    let rec trieStartsWith' cs trie =
        match cs with
        | [] -> true
        | h :: t ->
            match trie with
            | Leaf -> false
            | Node(m, isLeaf) ->
                if isLeaf then
                    true
                else
                    match Map.tryFind h m with
                    | None -> false
                    | Some(child) ->
                        trieStartsWith' t child

    trieStartsWith' (word |>Seq.toList) trie

let t1 = trieInsert "apple" Leaf
// true
trieSearch "apple" t1
// false
trieSearch "app" t1
// true
trieStartsWith "app" t1

let t2 = trieInsert "app" t1
// true
trieSearch "app" t2
