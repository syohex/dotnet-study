type ListNode =
    | Leaf
    | Node of int * ListNode

let addTwoNumbers (l1: ListNode) (l2: ListNode) : ListNode =
    let toReverseNums lst =
        let rec toReverseNums' lst acc =
            match lst with
            | Leaf -> acc
            | Node(v, n) -> toReverseNums' n (v :: acc)

        toReverseNums' lst []

    let rec numsToList nums =
        match nums with
        | [] -> Leaf
        | h :: t -> Node(h, numsToList t)

    let rec addTwoNumbers' nums1 nums2 carry acc =
        match nums1, nums2 with
        | [], [] ->
            if carry >= 1 then
                numsToList (carry :: acc)
            else
                numsToList acc
        | h :: t, [] ->
            let sum = h + carry

            if sum >= 10 then
                addTwoNumbers' t [] 1 ((sum % 10) :: acc)
            else
                addTwoNumbers' t [] 0 (sum :: acc)
        | [], h :: t ->
            let sum = h + carry

            if sum >= 10 then
                addTwoNumbers' [] t 1 ((sum % 10) :: acc)
            else
                addTwoNumbers' [] t 0 (sum :: acc)
        | h1 :: t1, h2 :: t2 ->
            let sum = h1 + h2 + carry

            if sum >= 10 then
                addTwoNumbers' t1 t2 1 ((sum % 10) :: acc)
            else
                addTwoNumbers' t1 t2 0 (sum :: acc)

    let nums1, nums2 = toReverseNums l1, toReverseNums l2
    addTwoNumbers' nums1 nums2 0 []


let list11 = Node(7, Node(2, Node(4, Node(3, Leaf))))
let list12 = Node(5, Node(6, Node(4, Leaf)))
// [7, 8, 0, 7]
addTwoNumbers list11 list12

let list21 = Node(2, Node(4, Node(3, Leaf)))
let list22 = Node(5, Node(6, Node(4, Leaf)))
// [8, 0, 7]
addTwoNumbers list21 list22

let list31 = Node(5, Leaf)
let list32 = Node(5, Leaf)
// [1, 0]
addTwoNumbers list31 list32

let list41 = Node(0, Leaf)
let list42 = Node(0, Leaf)
// [0]
addTwoNumbers list41 list42
