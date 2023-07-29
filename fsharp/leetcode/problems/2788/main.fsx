open System

let splitWordsBySeparator (words: string list) (separator: char) : string list =
    let rec splitWord (word: char list) separator (acc: char list) (ret: string list) =
        match word with
        | [] ->
            if List.isEmpty acc then
                List.rev ret
            else
                let s = acc |> List.rev |> String.Concat
                List.rev (s :: ret)
        | h :: t ->
            if h = separator then
                if List.isEmpty acc then
                    splitWord t separator acc ret
                else
                    let s = acc |> List.rev |> String.Concat
                    splitWord t separator [] (s :: ret)
            else
                splitWord t separator (h :: acc) ret

    let rec splitWordsBySeparator' words separator acc =
        match words with
        | [] -> acc
        | h :: t ->
            let ws = splitWord h separator [] []
            splitWordsBySeparator' t separator (acc @ ws)

    let words' = words |> List.map Seq.toList
    splitWordsBySeparator' words' separator []

// ["one","two","three","four","five","six"]
splitWordsBySeparator [ "one.two.three"; "four.five"; "six" ] '.'

// ["easy", "problem"]
splitWordsBySeparator [ "$easy$"; "$problem$" ] '$'

// []
splitWordsBySeparator [ "|||" ] '|'
