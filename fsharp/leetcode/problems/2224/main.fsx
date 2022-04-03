let timeToMinutes (s: string) : int =
    let ca = s |> Seq.toArray |> Array.map (fun b -> int b - int '0')
    (ca.[0] * 10 + ca.[1]) * 60 + ca.[3] * 10 + ca.[4]

let convertTime (current: string) (correct: string) : int =
    let diff = (timeToMinutes correct) - (timeToMinutes current)

    let ret = diff / 60
    let diff = diff % 60

    let ret = ret + diff / 15
    let diff = diff % 15

    let ret = ret + diff / 5
    let diff = diff % 5

    ret + diff

// 3
convertTime "02:30" "04:35"

// 1
convertTime "11:00" "11:01"

// 32
convertTime "00:00" "23:59"