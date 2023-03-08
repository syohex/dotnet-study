open System

let minEatingSpeed (piles: int list) (h: int) : int =
    let check (piles: int[]) bananas h =
        let hours =
            Array.fold (fun acc n -> acc + (int (Math.Ceiling(double n / double bananas)))) 0 piles

        hours <= h

    let rec minEatingSpeed' (piles: int[]) left right =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if check piles mid h then
                minEatingSpeed' piles left mid
            else
                minEatingSpeed' piles (mid + 1) right

    let piles' = piles |> List.sort |> Array.ofList
    minEatingSpeed' piles' 1 (piles'.[piles'.Length - 1])

// 4
minEatingSpeed [ 3; 6; 7; 11 ] 8

// 30
minEatingSpeed [ 30; 11; 23; 4; 20 ] 5

// 23
minEatingSpeed [ 30; 11; 23; 4; 20 ] 6
