type MyList =
    | Nil
    | ListNode of int * MyList

type Tree =
    | Leaf
    | TreeNode of int * Tree * Tree

let rec isSubPath (head: MyList) (root: Tree) : bool =
    let rec dfs list node =
        match list with
        | Nil -> true
        | ListNode(v1, next) ->
            match node with
            | Leaf -> false
            | TreeNode(v2, left, right) -> if v1 = v2 then dfs next left || dfs next right else false

    match root with
    | Leaf -> false
    | TreeNode(_, left, right) -> dfs head root || isSubPath head left || isSubPath head right

let list1 = ListNode(4, ListNode(2, ListNode(8, Nil)))

let tree1 =
    TreeNode(
        1,
        TreeNode(4, Leaf, TreeNode(2, TreeNode(1, Leaf, Leaf), Leaf)),
        TreeNode(
            4,
            TreeNode(2, TreeNode(6, Leaf, Leaf), TreeNode(8, TreeNode(1, Leaf, Leaf), TreeNode(3, Leaf, Leaf))),
            Leaf
        )
    )
// true
isSubPath list1 tree1

let list2 = ListNode(1, ListNode(4, ListNode(2, ListNode(6, Nil))))
// true
isSubPath list2 tree1

let list3 = ListNode(1, ListNode(4, ListNode(2, ListNode(6, ListNode(8, Nil)))))
// false
isSubPath list3 tree1

let list4 = ListNode(1, ListNode(10, Nil))

let tree4 =
    TreeNode(1, Leaf, TreeNode(1, TreeNode(10, TreeNode(9, Leaf, Leaf), Leaf), TreeNode(1, Leaf, Leaf)))
// true
isSubPath list4 tree4

let list5 = ListNode(2, ListNode(2, ListNode(1, Nil)))

let tree5 =
    TreeNode(2, Leaf, TreeNode(2, Leaf, TreeNode(2, Leaf, TreeNode(1, Leaf, Leaf))))
// true
isSubPath list5 tree5
