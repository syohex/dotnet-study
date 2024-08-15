let lemonadeChange (bills: int list) : bool =
    let rec lemonadeChange' bills fives tens =
        match bills with
        | [] -> true
        | h :: t ->
            match h with
            | 5 -> lemonadeChange' t (fives + 1) tens
            | 10 ->
                if fives < 1 then
                    false
                else
                    lemonadeChange' t (fives - 1) (tens + 1)
            | _ ->
                if fives >= 1 && tens >= 1 then
                    lemonadeChange' t (fives - 1) (tens - 1)
                elif fives >= 3 then
                    lemonadeChange' t (fives - 3) tens
                else
                    false

    lemonadeChange' bills 0 0

// true
lemonadeChange [ 5; 5; 5; 10; 20 ]

// false
lemonadeChange [ 5; 5; 10; 10; 20 ]

// true
lemonadeChange [ 5; 5; 10; 20; 5; 5; 5; 5; 5; 5; 5; 5; 5; 10; 5; 5; 20; 5; 20; 5 ]
