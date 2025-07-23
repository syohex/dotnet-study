let maximumGain (s: string) (x: int) (y: int) : int =
    let rec f cs c1 c2 score acc total =
        match cs with
        | [] -> List.rev acc, total
        | h :: t ->
            if h = c2 then
                match acc with
                | [] -> f t c1 c2 score (h :: acc) total
                | h1 :: t1 ->
                    if h1 = c1 then
                        f t c1 c2 score t1 (total + score)
                    else
                        f t c1 c2 score (h :: acc) total
            else
                f t c1 c2 score (h :: acc) total

    let cs = s |> Seq.toList

    if x > y then
        let cs, total1 = f cs 'a' 'b' x [] 0
        let _, total2 = f cs 'b' 'a' y [] 0
        total1 + total2
    else
        let cs, total1 = f cs 'b' 'a' y [] 0
        let _, total2 = f cs 'a' 'b' x [] 0
        total1 + total2

// 19
maximumGain "cdbcbbaaabab" 4 5

// 20
maximumGain "aabbaaxybbaabb" 5 4
