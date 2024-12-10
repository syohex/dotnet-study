let maximumLength (s: string) : int =
    let isSingleChar cs =
        match cs with
        | [] -> failwith "never reach here"
        | _ :: [] -> true
        | h :: t -> List.forall ((=) h) t

    let rec maximumLength' len cs acc =
        if len >= s.Length then
            acc
        else
            let ok =
                List.windowed len cs
                |> List.filter isSingleChar
                |> List.countBy id
                |> List.exists (fun (_, count) -> count >= 3)

            maximumLength' (len + 1) cs (if ok then len else acc)

    maximumLength' 1 (Seq.toList s) -1

// 2
maximumLength "aaaa"

// -1
maximumLength "abcdef"

// 1
maximumLength "abcaba"
