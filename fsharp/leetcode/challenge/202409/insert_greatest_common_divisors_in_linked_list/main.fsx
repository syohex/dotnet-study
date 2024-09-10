type MyList =
    | Nil
    | Node of int * MyList

let gcd (a: int) (b: int) : int =
    let rec gcd' a b =
        let m = a % b
        if m = 0 then b else gcd' b m

    gcd' (max a b) (min a b)

let rec insertGreatestCommonDivisors (head: MyList) : MyList =
    match head with
    | Nil -> Nil
    | Node(v1, next) ->
        match next with
        | Nil -> head
        | Node(v2, _) ->
            let d = gcd v1 v2
            Node(v1, Node(d, insertGreatestCommonDivisors next))

let list1 = Node(18, Node(6, Node(10, Node(3, Nil))))
// [18,6,6,2,10,1,3]
insertGreatestCommonDivisors list1

let list2 = Node(7, Nil)
// [7]
insertGreatestCommonDivisors list2
