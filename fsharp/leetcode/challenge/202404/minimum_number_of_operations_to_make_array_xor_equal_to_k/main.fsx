let minOperations (nums: int list) (k: int) : int =
    let rec minOperations' total k ret =
        if total = 0 && k = 0 then
            ret
        else if (total &&& 1) = (k &&& 1) then
            minOperations' (total >>> 1) (k >>> 1) ret
        else
            minOperations' (total >>> 1) (k >>> 1) (ret + 1)

    let total = List.reduce (^^^) nums
    minOperations' total k 0

// 2
minOperations [ 2; 1; 3; 4 ] 1

// 0
minOperations [ 0; 2; 0; 2 ] 0
