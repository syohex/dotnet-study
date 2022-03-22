let getSmallestString (n: int) (k: int) : string =
    let rec getSmallestString' (n: int) (k: int) (acc: char list) =
        if n = 0 then
            acc |> System.String.Concat
        else
            let v = System.Math.Min(26, (k - n + 1))
            getSmallestString' (n - 1) (k - v) ((char (v + int 'a' - 1)) :: acc)

    getSmallestString' n k []

// "aay"
getSmallestString 3 27

// "aaszz"
getSmallestString 5 73

// "aadzzzzzzzzzzzzzzzzzzzzz"
getSmallestString 24 552
