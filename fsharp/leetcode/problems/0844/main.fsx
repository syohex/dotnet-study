let backspaceCompare (s: string) (t: string) : bool =
    let rec build (cs: char list) (acc: char list) =
        match cs with
        | [] -> acc |> System.String.Concat
        | h :: t ->
            if h = '#' then
                build t (acc |> List.tail)
            else
                build t (h :: acc)

    (build (s |> Seq.toList) []) = (build (t |> Seq.toList) [])

// true
backspaceCompare "ab#c" "ad#c"

// true
backspaceCompare "ab##" "c#d#"

// false
backspaceCompare "a#c" "b"
