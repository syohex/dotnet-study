type MyHashMap =
    { Table: (int * int) list [] }

    static member init() : MyHashMap =
        { Table = Array.init 4096 (fun _ -> []) }

    static member hash(key: int) : int = key % 4096

    member this.put (key: int) (value: int) =
        let idx = MyHashMap.hash key

        match List.tryFindIndex (fun (k, _) -> k = key) this.Table.[idx] with
        | None -> this.Table.[idx] <- ((key, value) :: this.Table.[idx])
        | Some (i) -> this.Table.[idx] <- List.updateAt i (key, value) this.Table.[idx]

    member this.get(key: int) : int =
        let idx = MyHashMap.hash key

        match List.tryFind (fun (k, _) -> k = key) this.Table.[idx] with
        | None -> -1
        | Some ((_, v)) -> v

    member this.remove(key: int) =
        let idx = MyHashMap.hash key

        match List.tryFindIndex (fun (k, _) -> k = key) this.Table.[idx] with
        | None -> ()
        | Some (i) -> this.Table.[idx] <- List.removeAt i this.Table.[idx]

let h = MyHashMap.init ()
h.put 1 1
h.put 2 2
// 1
h.get 1
// -1
h.get 3
h.put 2 1
// 1
h.get 2
h.remove 2
// -1
h.get 2
