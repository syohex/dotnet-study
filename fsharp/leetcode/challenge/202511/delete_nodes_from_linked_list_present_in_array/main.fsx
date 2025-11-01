type ListNode =
    | Nil
    | Node of int * ListNode

let modifiedList (nums: int list) (head: ListNode) : ListNode =
    let rec modifiedList' node nums =
        match node with
        | Nil -> Nil
        | Node(v, next) ->
            if Set.contains v nums then
                modifiedList' next nums
            else
                Node(v, modifiedList' next nums)

    modifiedList' head (Set.ofList nums)

let list1 = Node(1, Node(2, Node(3, Node(4, Node(5, Nil)))))
// [4, 5]
modifiedList [ 1; 2; 3 ] list1

let list2 = Node(1, Node(2, Node(1, Node(2, Node(1, Node(2, Nil))))))
// [2,2,2]
modifiedList [ 1 ] list2

let list3 = Node(1, Node(2, Node(3, Node(4, Nil))))
// [1,2,3,4]
modifiedList [ 5 ] list3

let list4 = Node(2, Node(10, Node(9, Nil)))
// [10]
modifiedList [ 9; 2; 5 ] list4
