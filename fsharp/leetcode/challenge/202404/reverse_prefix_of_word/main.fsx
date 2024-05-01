open System

let reversePrefix (word: string) (ch: char) : string =
    let rec reversePrefix' (cs: char list) ch acc =
        match cs with
        | [] -> word
        | h :: t ->
            if h = ch then
                (h :: acc) @ t |> String.Concat
            else
                reversePrefix' t ch (h :: acc)

    reversePrefix' (Seq.toList word) ch []

// "dcbaefd"
reversePrefix "abcdefd" 'd'

// "zxyxxe"
reversePrefix "xyxzxe" 'z'

// "abcd"
reversePrefix "abcd" 'z'
