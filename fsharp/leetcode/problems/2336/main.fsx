type SmallestInfiniteSet =
    { Popped: Set<int> }

    static member init() = { Popped = Set.empty }

    static member popSmallest(s: SmallestInfiniteSet) : SmallestInfiniteSet * int =
        let rec popSmallest' i popped =
            if Set.contains i popped then
                popSmallest' (i + 1) popped
            else
                { Popped = (Set.add i popped) }, i

        popSmallest' 1 s.Popped

    static member addBack (num: int) (s: SmallestInfiniteSet) : SmallestInfiniteSet =
        let popped = s.Popped

        if Set.contains num popped then
            { Popped = (Set.remove num popped) }
        else
            s

let s = SmallestInfiniteSet.init ()
let s1 = SmallestInfiniteSet.addBack 2 s

// 1
let s2, tmp1 = SmallestInfiniteSet.popSmallest s1
// 2
let s3, tmp2 = SmallestInfiniteSet.popSmallest s2
// 3
let s4, tmp3 = SmallestInfiniteSet.popSmallest s3

let s5 = SmallestInfiniteSet.addBack 1 s4

// 1
let s6, tmp4 = SmallestInfiniteSet.popSmallest s5
// 4
let s7, tmp5 = SmallestInfiniteSet.popSmallest s6
// 5
let _, tmp6 = SmallestInfiniteSet.popSmallest s7
