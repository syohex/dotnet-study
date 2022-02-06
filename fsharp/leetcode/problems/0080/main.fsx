let removeDuplicates (nums: int []) : int =
    let rec removeDuplicates' i prev dups index =
        if i >= (Array.length nums) then
            index
        else if nums.[i] = prev then
            if dups >= 2 then
                removeDuplicates' (i + 1) prev (dups + 1) index
            else
                nums.[index] <- prev
                removeDuplicates' (i + 1) prev (dups + 1) (index + 1)
        else
            nums.[index] <- nums.[i]
            removeDuplicates' (i + 1) nums.[i] 1 (index + 1)

    removeDuplicates' 0 System.Int32.MaxValue 0 0

// ret=5, after=[1,1,1,2,2,3]
let arr1 = [| 1; 1; 1; 2; 2; 3 |]
let ret1 = removeDuplicates arr1
arr1 |> Array.take ret1

// ret=7, after=[0,0,1,1,2,3,3]
let arr2 = [| 0; 0; 1; 1; 1; 1; 2; 3; 3 |]
let ret2 = removeDuplicates arr2
arr2 |> Array.take ret2
