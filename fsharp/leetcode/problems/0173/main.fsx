type Tree =
    | Leaf
    | Node of int * Tree * Tree

type BSTIterator =
    { Nums: int []
      mutable Index: int }

    static member New(root: Tree) : BSTIterator =
        let rec collect node acc =
            match node with
            | Leaf -> acc
            | Node (n, left, right) ->
                let acc' = collect left acc
                collect right (n :: acc')

        let nums =
            collect root [] |> List.rev |> List.toArray

        { Nums = nums; Index = 0 }

    member this.Next() : int =
        let ret = this.Nums.[this.Index]
        this.Index <- this.Index + 1
        ret

    member this.HasNext() : bool = this.Index < this.Nums.Length

let tree =
    Node(7, Node(3, Leaf, Leaf), Node(15, Node(9, Leaf, Leaf), Node(20, Leaf, Leaf)))

let b = BSTIterator.New tree
// 3
b.Next()
// 7
b.Next()
// true
b.HasNext()
// 9
b.Next()
// true
b.HasNext()
// 15
b.Next()
// true
b.HasNext()
// 20
b.Next()
// false
b.HasNext()
