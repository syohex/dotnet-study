type NumArray =
    { Len: int
      Tree: int [] }

    static member init(nums: int []) : NumArray =
        let len = nums.Length
        let tree = Array.zeroCreate (2 * len)

        for i in len .. (2 * len - 1) do
            tree.[i] <- nums.[i - len]

        seq { 0 .. (len - 1) }
        |> Seq.rev
        |> Seq.iter (fun i -> tree.[i] <- tree.[2 * i] + tree.[2 * i + 1])

        { Len = len; Tree = tree }

    static member update (index: int) (value: int) (na: NumArray) : unit =
        let rec update' index (na: NumArray) =
            if index <= 0 then
                ()
            else
                let left, right =
                    if index % 2 = 1 then
                        index - 1, index
                    else
                        index, index + 1

                let parent = index / 2
                na.Tree.[parent] <- na.Tree.[left] + na.Tree.[right]
                update' parent na

        na.Tree.[na.Len + index] <- value
        update' (na.Len + index) na

    static member sumRange (left: int) (right: int) (na: NumArray) : int =
        let rec sumRange' left right (na: NumArray) acc =
            if left > right then
                acc
            else
                let acc', left' =
                    if left % 2 = 1 then
                        acc + na.Tree.[left], left + 1
                    else
                        acc, left

                let acc'', right' =
                    if right % 2 = 0 then
                        acc' + na.Tree.[right], right - 1
                    else
                        acc', right

                sumRange' (left' / 2) (right' / 2) na acc''

        sumRange' (left + na.Len) (right + na.Len) na 0

let na = NumArray.init [| 1; 3; 5 |]
// 9
NumArray.sumRange 0 2 na

NumArray.update 1 2 na

// 8
NumArray.sumRange 0 2 na
