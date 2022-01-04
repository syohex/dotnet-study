let toNumList (num: string) : int list =
    num |> Seq.toList |> List.map (fun c -> int c - int '0') |> List.rev

let addStrings (num1: string) (num2: string) : string =
    let rec addStrings' (num1: int list) (num2: int list) (carry: int) (acc: int list): int list =
        match (num1, num2, carry) with
        | ([], [], 0) -> acc
        | ([], [], n) -> addStrings' num1 num2 0 (n :: acc)
        | (h1 :: t1, [], n) -> addStrings' t1 [] (if h1 + n >= 10 then 1 else 0) (((h1 + n) % 10) :: acc)
        | ([], h2 :: t2, n) -> addStrings' [] t2 (if h2 + n >= 10 then 1 else 0) (((h2 + n) % 10) :: acc)
        | (h1 :: t1, h2 :: t2, n) -> addStrings' t1 t2 (if h1 + h2 + n >= 10 then 1 else 0) (((h1 + h2 + n) % 10) :: acc)

    addStrings' (toNumList num1) (toNumList num2) 0 []
    |> List.map (fun n -> n.ToString())
    |> String.concat ""

toNumList "123"

addStrings "11" "123"
addStrings "456" "77"
addStrings "0" "0"
addStrings "99999" "99"
