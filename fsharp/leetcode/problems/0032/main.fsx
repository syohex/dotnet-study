open System

let longestValidParentheses (s: string) : int =
    let rec longestValidParentheses' cs stack (ret: int) =
        match cs with
        | [] -> ret
        | (i, h) :: t ->
            if h = '(' then
                longestValidParentheses' t (i :: stack) ret
            else
                let stack' =
                    match stack with
                    | [] -> []
                    | _ :: rest -> rest

                match stack' with
                | [] -> longestValidParentheses' t [ i ] ret
                | top :: _ ->
                    let ret' = Math.Max(ret, i - top)
                    longestValidParentheses' t stack' ret'

    let cs = s |> Seq.toList |> List.mapi (fun i c -> i, c)
    longestValidParentheses' cs [ -1 ] 0

// 2
longestValidParentheses "(()"

// 4
longestValidParentheses ")()())"

// 0
longestValidParentheses ""

// 6
longestValidParentheses "()(())"
