open System.Numerics

let countBits (n: int) : int list =
    let hammingWeight n =
        let m = n
        let m = (m &&& 0x55555555) + ((m >>> 1) &&& 0x55555555)
        let m = (m &&& 0x33333333) + ((m >>> 2) &&& 0x33333333)
        let m = (m &&& 0x0F0F0F0F) + ((m >>> 4) &&& 0x0F0F0F0F)
        let m = (m &&& 0x00FF00FF) + ((m >>> 8) &&& 0x00FF00FF)
        (m &&& 0x0000FFFF) + ((m >>> 16) &&& 0x0000FFFF)

    let rec countBits' n acc =
        if n < 0 then
            acc
        else
            let bits = hammingWeight n
            countBits' (n - 1) (bits :: acc)

    countBits' n []

let countBits2 (n: int) : int list =
    seq { 0..n }
    |> Seq.map (fun m -> BitOperations.PopCount(uint32 m))
    |> Seq.toList

// [0, 1, 1]
countBits 2
countBits2 2

// [0,1,1,2,1,2]
countBits 5
countBits2 5
