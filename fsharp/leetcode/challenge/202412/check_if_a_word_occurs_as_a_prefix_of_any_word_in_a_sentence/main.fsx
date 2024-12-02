let isPrefixOfWord (sentence: string) (searchWord: string) : int =
    sentence.Split(' ')
    |> Array.tryFindIndex (fun s -> s.StartsWith(searchWord))
    |> Option.map ((+) 1)
    |> Option.defaultValue -1

// 4
isPrefixOfWord "i love eating burger" "burg"

// 2
isPrefixOfWord "this problem is an easy problem" "pro"

// -1
isPrefixOfWord "i am tired" "you"
