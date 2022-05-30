open System

let divide (dividend: int) (divisor: int) : int =
    if divisor = 1 then
        dividend
    elif divisor = -1 then
        if dividend = Int32.MinValue then
            Int32.MaxValue
        else
            -dividend
    else
        let dividend = int64 dividend
        let divisor = int64 divisor

        let a, b, sign =
            match dividend >= 0, divisor >= 0 with
            | true, true -> dividend, divisor, 1
            | true, false -> dividend, -divisor, -1
            | false, true -> -dividend, divisor, -1
            | false, false -> -dividend, -divisor, 1

        let rec divide' a b acc =
            if a < b then
                acc
            else
                divide' (a - b) b (acc + 1)

        (divide' a b 0) * sign

// 3
divide 10 3

// -2
divide 7 -3

// 2147483647
divide Int32.MinValue -1

// -1073741824
divide Int32.MinValue 2
