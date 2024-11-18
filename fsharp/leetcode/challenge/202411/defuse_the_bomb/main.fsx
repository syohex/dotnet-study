let decrypt (code: int[]) (k: int) : int[] =
    let len = Array.length code

    if k = 0 then
        Array.zeroCreate len
    else if k > 0 then
        [| 0 .. (len - 1) |]
        |> Array.map (fun i -> seq { 1..k } |> Seq.fold (fun acc j -> acc + code.[(i + j) % len]) 0)
    else
        let k = -k

        [| 0 .. (len - 1) |]
        |> Array.map (fun i -> seq { 1..k } |> Seq.fold (fun acc j -> acc + code.[(len + i - j) % len]) 0)

//  [12,10,16,13]
decrypt [| 5; 7; 1; 4 |] 3

// [0,0,0,0]
decrypt [| 1; 2; 3; 4 |] 0

// [12,5,6,13]
decrypt [| 2; 4; 9; 3 |] -2
