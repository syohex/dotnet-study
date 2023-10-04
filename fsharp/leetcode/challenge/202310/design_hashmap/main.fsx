type MyHashMap =
    { Table: (int * int) list[] }

    static member private TableSize = 256

    static member private hash(key: int) : int = key % MyHashMap.TableSize

    static member init() : MyHashMap =
        { Table = Array.init MyHashMap.TableSize (fun _ -> [])}

    static member put (key: int) (value: int) (hash: MyHashMap) : MyHashMap =
        let rec put' lst key value =
            match lst with
            | [] -> [ (key, value) ]
            | (k, v) :: t ->
                if k = key then
                    (k, value) :: t
                else
                    (k, v) :: (put' t key value)

        let h = MyHashMap.hash key
        hash.Table.[h] <- put' hash.Table.[h] key value
        { Table = hash.Table }

    static member get (key: int) (hash: MyHashMap) : int =
        let h = MyHashMap.hash key

        match List.tryFind (fun (k, _) -> k = key) hash.Table.[h] with
        | None -> -1
        | Some((_, v)) -> v


    static member remove (key: int) (hash: MyHashMap) : MyHashMap =
        let rec remove' lst key =
            match lst with
            | [] -> []
            | (k, v) :: t -> if k = key then t else (k, v) :: (remove' t key)

        let h = MyHashMap.hash key
        hash.Table.[h] <- remove' hash.Table.[h] key
        { Table = hash.Table }

let h1 = MyHashMap.init ()
let h2 = h1 |> MyHashMap.put 1 1 |> MyHashMap.put 2 2
// 1
MyHashMap.get 1 h2
// -1
MyHashMap.get 3 h2

let h3 = MyHashMap.put 2 1 h2
// 1
MyHashMap.get 2 h3

let h4 = MyHashMap.remove 2 h3

// -1
MyHashMap.get 2 h4
