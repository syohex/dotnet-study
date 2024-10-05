let checkInclusion (s1: string) (s2: string) : bool =
    let table = s1 |> Seq.countBy id |> Seq.sort |> Seq.toList

    s2
    |> Seq.windowed s1.Length
    |> Seq.map (Seq.countBy id >> Seq.sort >> Seq.toList)
    |> Seq.contains table

// true
checkInclusion "ab" "eidbaooo"

// false
checkInclusion "ab" "eidboaoo"

// true
checkInclusion "a" "ab"

// false
checkInclusion "ab" "a"
