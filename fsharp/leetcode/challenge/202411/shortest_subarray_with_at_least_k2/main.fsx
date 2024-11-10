open System

let setBits (n: int) (bits: int[]) =
    seq { 0..31 } |> Seq.iter (fun i -> bits.[i] <- bits.[i] + ((n >>> i) &&& 1))

let unsetBits (n: int) (bits: int[]) =
    seq { 0..31 } |> Seq.iter (fun i -> bits.[i] <- bits.[i] - ((n >>> i) &&& 1))

let orBits (bits: int[]) : int =
    bits
    |> Array.indexed
    |> Array.fold (fun acc (i, n) -> if n = 0 then acc else acc ||| (1 <<< i)) 0

let rec adjustWindowsSize left right (nums: int[]) (bits: int[]) acc k =
    if left > right || (orBits bits < k) then
        left, acc
    else
        let acc = min acc (right - left + 1)
        unsetBits nums.[left] bits
        adjustWindowsSize (left + 1) right nums bits acc k

let minimumArrayLength (nums: int list) (k: int) : int =
    let rec minimumArrayLength' left right (nums: int[]) (bits: int[]) acc =
        if right >= nums.Length then
            if acc = Int32.MaxValue then -1 else acc
        else
            setBits nums.[right] bits
            let left, acc = adjustWindowsSize left right nums bits acc k
            minimumArrayLength' left (right + 1) nums bits acc

    minimumArrayLength' 0 0 (List.toArray nums) (Array.zeroCreate 32) Int32.MaxValue

// 1
minimumArrayLength [ 1; 2; 3 ] 2

// 3
minimumArrayLength [ 2; 1; 8 ] 10

// 1
minimumArrayLength [ 1; 2 ] 0
