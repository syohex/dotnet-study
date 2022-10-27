let haveConflict (event1: string * string) (event2: string * string) : bool =
    let toSecond (s: string) : int =
        let cs = Array.ofSeq s
        let min10 = int cs.[0] - int '0'
        let min1 = int cs.[1] - int '0'
        let sec10 = int cs.[3] - int '0'
        let sec1 = int cs.[4] - int '0'

        (min10 * 10 + min1) * 60 + (sec10 * 10) + sec1

    let start1 = fst event1 |> toSecond
    let end1 = snd event1 |> toSecond
    let start2 = fst event2 |> toSecond
    let end2 = snd event2 |> toSecond

    if start1 <= start2 then
        start2 <= end1
    else
        start1 <= end2


// true
haveConflict ("01:15", "02:00") ("02:00", "03:00")
// true
haveConflict ("02:00", "03:00") ("01:15", "02:00")

// true
haveConflict ("01:00", "02:00") ("01:20", "03:00")
// true
haveConflict ("01:20", "03:00") ("01:00", "02:00")

// false
haveConflict ("10:00", "11:00") ("14:00", "15:00")
// false
haveConflict ("14:00", "15:00") ("10:00", "11:00")
