let canPutBalls position force m =
    let rec canPutBalls' position force prev balls =
        match position with
        | [] -> false
        | h :: t ->
            if h - prev >= force then
                let balls' = balls + 1
                if balls' = m then true else canPutBalls' t force h balls'
            else
                canPutBalls' t force prev balls

    match position with
    | [] -> false
    | h :: t -> canPutBalls' t force h 1

let maxDistance (position: int list) (m: int) =
    let rec maxDistance' left right position ret =
        if left > right then
            ret
        else
            let mid = left + (right - left) / 2

            if canPutBalls position mid m then
                maxDistance' (mid + 1) right position mid
            else
                maxDistance' left (mid - 1) position ret

    let position' = List.sort position
    let diff = (List.last position') - (List.head position')
    let right = System.Math.Ceiling(double diff / (double m - 1.0)) |> int
    maxDistance' 1 right position' 0

// 3
maxDistance [ 1; 2; 3; 4; 7 ] 3

// 999999999
maxDistance [ 5; 4; 3; 2; 1; 1000000000 ] 2
