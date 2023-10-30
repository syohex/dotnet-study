let sortByBits (arr: int list) : int list =
    let popCount n =
        let n = (n &&& 0x55555555) + ((n >>> 1) &&& 0x55555555)
        let n = (n &&& 0x33333333) + ((n >>> 2) &&& 0x33333333)
        let n = (n &&& 0x0F0F0F0F) + ((n >>> 4) &&& 0x0F0F0F0F)
        let n = (n &&& 0x00FF00FF) + ((n >>> 8) &&& 0x00FF00FF)
        (n &&& 0x0000FFFF) + ((n >>> 16) &&& 0x0000FFFF)

    arr
    |> List.map (fun n -> n, popCount n)
    |> List.sortWith (fun (a1, b1) (a2, b2) -> if b1 = b2 then compare a1 a2 else compare b1 b2)
    |> List.map fst

// [1;2;4;8;3;5;6;7]
sortByBits [ 1; 2; 3; 4; 5; 6; 7; 8 ]

// [1,2,4,8,16,32,64,128,256,512,1024]
sortByBits [ 1024; 512; 256; 128; 64; 32; 16; 8; 4; 2; 1 ]
