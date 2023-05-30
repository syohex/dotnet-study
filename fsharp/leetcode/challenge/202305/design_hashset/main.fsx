type MyHashSet =
    { Table: int list[] }

    static member init() : MyHashSet = { Table = Array.init 1024 (fun _ -> [])}

    static member add (key: int) (s: MyHashSet) : MyHashSet =
        let index = (hash key) % 1024
        let lst = s.Table.[index]

        if List.contains key lst then
            s
        else
            let lst' = key :: lst
            s.Table.[index] <- lst'
            s

    static member remove (key: int) (s: MyHashSet) : MyHashSet =
        let index = (hash key) % 1024
        let lst = s.Table.[index]

        if List.contains key lst then
            let lst' = List.except [ key ] lst
            s.Table.[index] <- lst'
            s
        else
            s

    static member contains (key: int) (s: MyHashSet) : bool =
        let index = (hash key) % 1024
        let lst = s.Table.[index]
        List.contains key lst

let s = MyHashSet.init ()
let s1 = MyHashSet.add 1 s
let s2 = MyHashSet.add 2 s1
// true
MyHashSet.contains 1 s2
// false
MyHashSet.contains 3 s2
let s3 = MyHashSet.add 2 s1
// true
MyHashSet.contains 2 s3
let s4 = MyHashSet.remove 2 s3
// false
MyHashSet.contains 2 s4
