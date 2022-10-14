type MyList =
    | Leaf
    | Node of int * MyList

let deleteMiddle (head: MyList) : MyList =
    let rec deleteMiddle' node i =
        match node with
        | Leaf -> i, Leaf
        | Node (v, next) ->
            let len, lst = deleteMiddle' next (i + 1)

            if i = (len / 2) then
                len, next
            else
                len, Node(v, lst)

    deleteMiddle' head 0 |> snd

let list1 = Node(1, Node(3, Node(4, Node(7, Node(1, Node(2, Node(6, Leaf)))))))
// [1,3,4,1,2,6]
deleteMiddle list1

let list2 = Node(1, Node(2, Node(3, Node(4, Leaf))))
// [1,2,4]
deleteMiddle list2

let list3 = Node(2, Node(1, Leaf))
// [2]
deleteMiddle list3

let list4 = Node(1, Leaf)
// []
deleteMiddle list4
