open System

let capitalize (s: string) : string =
    let isAlphabet c =
        (int c >= int 'a' && int c <= int 'z')
        || (int c >= int 'A' && int c <= int 'Z')

    let rec capitalize' (cs: char list) nonWord (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            if isAlphabet h then
                if nonWord then
                    capitalize' t false (Char.ToUpper(h) :: acc)
                else
                    capitalize' t false (Char.ToLower(h) :: acc)
            else
                let nonWord' =
                    Char.IsPunctuation(h) || Char.IsWhiteSpace(h)

                capitalize' t nonWord' (h :: acc)

    capitalize' (s |> Seq.toList) true []


// "The C++ Challenger"
capitalize "the c++ challenger"

// "This Is An Example, Should Work!""
capitalize "THIS IS an ExampLE, should wORk!"
