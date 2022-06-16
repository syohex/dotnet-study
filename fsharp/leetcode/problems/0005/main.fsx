let longestPalindrome (s: string) : string =
    let len = s.Length
    let dp: bool [,] = Array2D.zeroCreate len len

    for i in 0 .. len - 1 do
        dp.[i, i] <- true

    let mutable start = 0
    let mutable last = 0

    for i in 0 .. len - 2 do
        if s.[i] = s.[i + 1] then
            dp.[i, i + 1] <- true
            start <- i
            last <- i + 1

    for i in 2 .. len do
        for j in 0 .. (len - i - 1) do
            if s.[j] = s.[j + i] && dp.[j + 1, j + i - 1] then
                dp.[j, j + i] <- true
                start <- j
                last <- j + i

    s.[start..last]

// "aba"
longestPalindrome "babad"

// "bb"
longestPalindrome "bb"

// "gytyg"
longestPalindrome
    "cwziydanrqvsdtvnnqgjnbrvvwxwqojeqgxhwxdoktjktulemwpbeqscbbtbfvkxsrjetfdrovcrdwzfmnnihtgxybuairswfewvpuscocqifuwylhssldpjrawqdrbvkykpaggspbfrulcktpbofchzikhzxhpocgvdbwpewpywsgqbczmamprklaoovcfecwchhmsaqkhvuvvzjblmgvqpqtnlipgqsanvovylpmxlmxvymppdykphhaamtxjnnlsqfwjwhyywgurteaummwhvavxbcpgrfffxrowluqmqjaugryxdmwvyokdcfcvcytxpixbvwrdgzctejdoaavgtezexmvxgrkpnayvfarkyoruofqmpnsqdzojxqrjsnfwsbzjmaoigytygukqlrcqaxazvmytgfghdczvzphfdbnxtklaiqqsotavdmhiaermluafheowcobjqmrkmlzyas"
