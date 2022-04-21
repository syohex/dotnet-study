type MyHashSet =
    { Table: int list [] }

    static member init() : MyHashSet =
        { Table = Array.init 512 (fun _ -> []) }

    member this.add(key: int) =
        let idx = MyHashSet.hash key

        match List.tryFind ((=) key) this.Table.[idx] with
        | Some (_) -> ()
        | None ->
            this.Table.[idx] <- (key :: this.Table.[idx])
            ()

    member this.remove(key: int) =
        let idx = MyHashSet.hash key

        match List.tryFindIndex ((=) key) this.Table.[idx] with
        | Some (i) ->
            this.Table.[idx] <- this.Table.[idx] |> List.removeAt i
            ()
        | None -> ()

    member this.contains(key: int) : bool =
        let idx = MyHashSet.hash key

        List.tryFind ((=) key) this.Table.[idx]
        |> Option.isSome


    static member hash(key: int) = (hash key) % 512


let h = MyHashSet.init ()
h.add 1
h.add 2
// true
h.contains 1
// false
h.contains 3
h.add 2
// true
h.contains 2
h.remove 2
// false
h.contains 2
