let addTwoNumbers (l1: int list) (l2: int list) : int list =
    let rec addTwoNumbers' (l1: int list) (l2: int list) (ret: int list) (carry: int) : int list =
        if List.isEmpty l1 && List.isEmpty l2 then
            if carry <> 0 then
                addTwoNumbers' l1 l2 (carry :: ret) 0
            else
                ret
        else
            let e1 = if List.isEmpty l1 then 0 else List.head l1
            let e2 = if List.isEmpty l2 then 0 else List.head l2
            let r1 = if List.isEmpty l1 then [] else List.tail l1
            let r2 = if List.isEmpty l2 then [] else List.tail l2
            match e1 + e2 + carry with
            | n when n >= 10 -> addTwoNumbers' r1 r2 ((n % 10) :: ret) 1
            | n -> addTwoNumbers' r1 r2 (n :: ret) 0

    addTwoNumbers' l1 l2 [] 0 |> List.rev

addTwoNumbers [ 2; 4; 3 ] [ 5; 6; 4 ]
addTwoNumbers [ 0 ] [ 0 ]
addTwoNumbers [ 9; 9; 9; 9 ] [ 9; 9 ]
