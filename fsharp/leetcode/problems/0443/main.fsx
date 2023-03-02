let compress (chars: char[]) : int =
    let countToStr count =
        let rec countToStr' count acc =
            if count = 0 then
                acc
            else
                let c = char (count % 10 + int '0')
                countToStr' (count / 10) (c :: acc)

        countToStr' count []

    let appendCharsCount (chars: char[]) count pos =
        let rec appendCharsCount' digits (chars: char[]) pos =
            match digits with
            | [] -> pos
            | h :: t ->
                chars.[pos] <- h
                appendCharsCount' t chars (pos + 1)

        let digits = countToStr count
        appendCharsCount' digits chars pos

    let rec compress' (chars: char[]) len i ch count pos =
        if i >= len then
            chars.[pos] <- ch

            if count > 1 then
                appendCharsCount chars count (pos + 1)
            else
                pos + 1
        else if ch = chars.[i] then
            compress' chars len (i + 1) ch (count + 1) pos
        else
            chars.[pos] <- ch

            if count = 1 then
                compress' chars len (i + 1) (chars.[i]) 1 (pos + 1)
            else
                let pos' = appendCharsCount chars count (pos + 1)
                compress' chars len (i + 1) (chars.[i]) 1 pos'


    compress' chars chars.Length 1 chars.[0] 1 0

let chars1 = [| 'a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'c' |]
// 6
let ret1 = compress chars1
// ["a","2","b","2","c","3"]
Array.take ret1 chars1

let chars2 = [| 'a' |]
// 1
let ret2 = compress chars2
// ['a']
Array.take ret2 chars2

let chars3 = [| 'a'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b'; 'b' |]
// 4
let ret3 = compress chars3
// ['a', 'b', '1', '2']
Array.take ret3 chars3
