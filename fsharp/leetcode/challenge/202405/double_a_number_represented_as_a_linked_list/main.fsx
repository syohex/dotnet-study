type MyList =
    | Leaf
    | Node of int * MyList

let doubleIt (head: MyList) : MyList =
    let rec doubleIt' node =
        match node with
        | Leaf -> Leaf, 0
        | Node(v, next) ->
            let lst, carry = doubleIt' next
            let v' = v * 2 + carry
            if v' >= 10 then Node(v' % 10, lst), 1 else Node(v', lst), 0

    let lst, carry = doubleIt' head
    if carry > 0 then Node(1, lst) else lst

let lst1 = Node(1, Node(8, Node(9, Leaf)))
// 378
doubleIt lst1

let lst2 = Node(9, Node(9, Node(9, Leaf)))
// 1998
doubleIt lst2
