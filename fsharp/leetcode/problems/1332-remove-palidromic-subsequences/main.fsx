let removePalidromeSub (s: string) : int =
    let r = s |> Seq.rev |> System.String.Concat
    if s = r then 1 else 2

// 1
removePalidromeSub "ababa"

// 2
removePalidromeSub "abb"

// 2
removePalidromeSub "baabb"
