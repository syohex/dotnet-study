let maximumGain (s: string) (x: int) (y: int) : int =
    let rec maximumGain' cs first second score total acc =
        match cs with
        | [] -> total, List.rev acc
        | a :: b :: t when a = first && b = second -> maximumGain' t first second score (total + score) acc
        | h :: t ->
            match acc with
            | [] -> maximumGain' t first second score total (h :: acc)
            | a :: rest ->
                if a = first && h = second then
                    maximumGain' t first second score (total + score) rest
                else
                    maximumGain' t first second score total (h :: acc)

    let cs = Seq.toList s

    if x > y then
        let ret1, cs' = maximumGain' cs 'a' 'b' x 0 []
        let ret2, _ = maximumGain' cs' 'b' 'a' y 0 []
        ret1 + ret2
    else
        let ret1, cs' = maximumGain' cs 'b' 'a' y 0 []
        let ret2, _ = maximumGain' cs' 'a' 'b' x 0 []
        ret1 + ret2

//  19
maximumGain "cdbcbbaaabab" 4 5

// 20
maximumGain "aabbaaxybbaabb" 5 4
