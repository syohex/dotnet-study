let nextGreatestLetter (letters: char[]) (target: char) : char =
    let rec nextGreatestLetter' left right (letters: char[]) =
        if left > right then
            if left >= letters.Length then
                letters.[0]
            else
                letters.[left]
        else
            let mid = left + (right - left) / 2

            if letters.[mid] > target then
                nextGreatestLetter' left (mid - 1) letters
            else
                nextGreatestLetter' (mid + 1) right letters

    nextGreatestLetter' 0 (letters.Length - 1) letters

// 'c'
nextGreatestLetter [| 'c'; 'f'; 'j' |] 'a'
// 'f'
nextGreatestLetter [| 'c'; 'f'; 'j' |] 'c'
// 'x'
nextGreatestLetter [| 'x'; 'x'; 'y'; 'y' |] 'z'
