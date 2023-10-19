let backspaceCompare (s: string) (t: string) : bool =
    let rec f cs skips acc =
        match cs with
        | [] -> acc
        | h :: t ->
            if h = '#' then f t (skips + 1) acc
            else if skips > 0 then f t (skips - 1) acc
            else f t skips (h :: acc)

    let fn = Seq.toList >> List.rev
    let s' = f (fn s) 0 []
    let t' = f (fn t) 0 []

    s' = t'

// true
backspaceCompare "ab#c" "ad#c"

// true
backspaceCompare "ab##" "c#d#"

//false
backspaceCompare "a#c" "b"

// false
backspaceCompare "bbbextm" "bbb#extm"

// true
backspaceCompare "nzp#o#g" "b#nzp#o#g"
