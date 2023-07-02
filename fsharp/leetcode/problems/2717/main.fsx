let semiorderPermutation (nums: int list) : int =
    let len = List.length nums
    let p1 = List.findIndex (fun n -> n = 1) nums
    let p2 = List.findIndex (fun n -> n = len) nums

    let ret = p1 + len - 1 - p2
    if p1 < p2 then ret else ret - 1

// 2
semiorderPermutation [ 2; 1; 4; 3 ] |> printfn "%A"

// 3
semiorderPermutation [ 2; 4; 1; 3 ] |> printfn "%A"

// 0
semiorderPermutation [ 1; 3; 4; 2; 5 ] |> printfn "%A"

// 1
semiorderPermutation [ 2; 1 ] |> printfn "%A"

// 0
semiorderPermutation [ 1; 2 ] |> printfn "%A"
