let minEatingSpeed (piles: int list) (h: int) : int =
    let rec minEatingSpeed' piles h left right =
        if left >= right then
            right
        else
            let mid = (left + right) / 2

            let sum =
                piles
                |> List.fold (fun acc n -> acc + (n / mid) + (if n % mid = 0 then 0 else 1)) 0

            if sum <= h then
                minEatingSpeed' piles h left mid
            else
                minEatingSpeed' piles h (mid + 1) right

    let max = piles |> List.max
    minEatingSpeed' piles h 1 max

minEatingSpeed [ 3; 6; 7; 11 ] 8
minEatingSpeed [ 30; 11; 23; 4; 20 ] 5
minEatingSpeed [ 30; 11; 23; 4; 20 ] 6
