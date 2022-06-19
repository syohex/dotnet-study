open System

type Trie =
    { table: Map<char, Trie>
      candidates: Set<string> }

    static member empty() : Trie =
        { table = Map.empty
          candidates = Set.empty }

let insertToTrie (word: string) (trie: Trie) : Trie =
    let rec insertToTrie' (cs: char list) word (trie: Trie) =
        match cs with
        | [] -> Trie.empty ()
        | h :: t ->
            let next =
                match Map.tryFind h trie.table with
                | None -> (Trie.empty ())
                | Some (v) -> v

            let trie' = insertToTrie' t word next

            let trie'' =
                { trie' with candidates = Set.add word trie'.candidates }

            { trie with table = Map.add h trie'' trie.table }

    insertToTrie' (word |> Seq.toList) word trie

let suggestedProducts (products: string list) (searchWord: string) : string list list =
    let rec suggestedProducts' (cs: char list) (trie: Trie) acc =
        match cs with
        | [] -> acc |> List.rev
        | h :: t ->
            match Map.tryFind h trie.table with
            | None -> suggestedProducts' t (Trie.empty ()) ([] :: acc)
            | Some (v) ->
                let len = Math.Min(3, Set.count v.candidates)

                let cands =
                    v.candidates
                    |> Set.toList
                    |> List.sort
                    |> List.take len

                suggestedProducts' t v (cands :: acc)

    let trie =
        products
        |> List.fold (fun acc word -> insertToTrie word acc) (Trie.empty ())

    suggestedProducts' (searchWord |> Seq.toList) trie []


// [
//   ["mobile","moneypot","monitor"],
//   ["mobile","moneypot","monitor"],
//   ["mouse","mousepad"],
//   ["mouse","mousepad"],
//   ["mouse","mousepad"]
// ]
suggestedProducts
    [ "mobile"
      "mouse"
      "moneypot"
      "monitor"
      "mousepad" ]
    "mouse"

// [["havana"],["havana"],["havana"],["havana"],["havana"],["havana"]]
suggestedProducts [ "havana" ] "havana"

// [["baggage","bags","banner"],["baggage","bags","banner"],["baggage","bags"],["bags"]]
suggestedProducts
    [ "bags"
      "baggage"
      "banner"
      "box"
      "cloths" ]
    "bags"

// [[],[],[],[],[],[]],[]
suggestedProducts ["havana"] "abcdefg"
