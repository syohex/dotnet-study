open System

let reverseParentheses (s: string) : string =
    let rec reverseParentheses' (cs: char list) acc =
        match cs with
        | [] -> List.rev acc, []
        | h :: t ->
            match h with
            | '(' ->
                let v, rest = reverseParentheses' t []
                reverseParentheses' rest (v @ acc)
            | ')' -> List.rev acc, t
            | c -> reverseParentheses' t (c :: acc)

    reverseParentheses' (Seq.toList s) [] |> fst |> String.Concat

// dcba
reverseParentheses "(abcd)"

// iloveu
reverseParentheses "(u(love)i)"

// leetcode
reverseParentheses "(ed(et(oc))el)"
