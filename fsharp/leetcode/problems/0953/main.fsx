let sort (a: string) (b: string) (dict: Map<char, int>) : int =
    let rec sort' a b dict =
        match a, b with
        | [], [] -> 0
        | _, [] -> 1
        | [], _ -> -1
        | h1 :: t1, h2 :: t2 ->
            if h1 <> h2 then
                compare (Map.find h1 dict) (Map.find h2 dict)
            else
                sort' t1 t2 dict

    sort' (Seq.toList a) (Seq.toList b) dict

let isAlienSorted (words: string list) (order: string) : bool =
    let dict =
        order |> Seq.indexed |> Seq.fold (fun acc (i, c) -> Map.add c i acc) Map.empty

    let sorted = List.sortWith (fun a b -> sort a b dict) words
    words = sorted

// true
isAlienSorted [ "hello"; "leetcode" ] "hlabcdefgijkmnopqrstuvwxyz"

// false
isAlienSorted [ "word"; "world"; "row" ] "worldabcefghijkmnpqstuvxyz"

// false
isAlienSorted [ "apple"; "app" ] "abcdefghijklmnopqrstuvwxyz"
