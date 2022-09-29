let findClosestElements (arr: int list) (k: int) (x: int) : int list =
    arr
    |> List.sortWith (fun a b ->
        let aAbs = System.Math.Abs(a - x)
        let bAbs = System.Math.Abs(b - x)

        if aAbs = bAbs then
            compare a b
        else
            compare aAbs bAbs)
    |> List.take k
    |> List.sort

// [1;2;3;4]
findClosestElements [ 1; 2; 3; 4; 5 ] 4 3

// [1;2;3;4]
findClosestElements [ 1; 2; 3; 4; 5 ] 4 -1
