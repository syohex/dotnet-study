let rangeBitwiseAnd (left: int) (right: int) : int =
    let rec rangeBitwiseAnd' left right shifts =
        if left = right then
            left <<< shifts
        else
            rangeBitwiseAnd' (left >>> 1) (right >>> 1) (shifts + 1)

    rangeBitwiseAnd' left right 0

// 4
rangeBitwiseAnd 5 7

// 0
rangeBitwiseAnd 0 0

// 0
rangeBitwiseAnd 1 System.Int32.MaxValue

// 6
rangeBitwiseAnd 6 7
