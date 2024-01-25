let longestCommonSubsequence (text1: string) (text2: string) : int =
    let len1, len2 = text1.Length, text2.Length
    let dp = Array2D.zeroCreate (len1 + 1) (len2 + 1)

    for i in seq { 0 .. (len1 - 1) } |> Seq.rev do
        for j in seq { 0 .. (len2 - 1) } |> Seq.rev do
            if text1.[i] = text2.[j] then
                dp.[i, j] <- dp.[i + 1, j + 1] + 1
            else
                dp.[i, j] <- System.Math.Max(dp.[i + 1, j], dp.[i, j + 1])

    dp.[0, 0]

// 3
longestCommonSubsequence "abcde" "ace"

// 3
longestCommonSubsequence "abc" "abc"

// 0
longestCommonSubsequence "abc" "def"
