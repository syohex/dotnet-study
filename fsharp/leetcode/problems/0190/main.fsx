let toBits (n: uint32) : uint32 list =
    let rec toBits' n count acc =
        if count = 0 then
            acc
        else
            let b = n % 2u
            toBits' (n >>> 1) (count - 1) (b :: acc)

    toBits' n 32 []

toBits 15u
    
let reverseBits (n: uint32) : uint32 =
    toBits n |> List.rev |> List.fold (fun acc b -> acc * 2u + b) 0u

reverseBits 43261596u
reverseBits 0b11111111111111111111111111111101u
