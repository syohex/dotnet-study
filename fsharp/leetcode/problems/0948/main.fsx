let bagOfTokensScore (tokens: int list) (power: int) : int =
    let rec bagOfTokensScore' left right (tokens: int []) power score ret =
        if left > right then
            ret
        elif power >= tokens.[left] then
            let score' = score + 1
            bagOfTokensScore' (left + 1) right tokens (power - tokens.[left]) score' (System.Math.Max(ret, score + 1))
        elif score >= 1 then
            bagOfTokensScore' left (right - 1) tokens (power + tokens.[right]) (score - 1) ret
        else
            ret

    let tokens' = tokens |> List.sort |> List.toArray
    bagOfTokensScore' 0 (tokens'.Length - 1) tokens' power 0 0

// 0
bagOfTokensScore [ 100 ] 50

// 1
bagOfTokensScore [ 100; 200 ] 150

// 2
bagOfTokensScore [ 100; 200; 300; 400 ] 200

// 0
bagOfTokensScore [ 71; 55; 82 ] 54

// 1
bagOfTokensScore [ 26 ] 51
