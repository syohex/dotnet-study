let canPlaceFlowers (flowerbed: int list) (n: int) : bool =
    let rec canPlaceFlowers' flowerbed n prev =
        if n = 0 then
            true
        else
            match flowerbed with
            | [] -> false
            | 0 :: [] when prev = 0 -> canPlaceFlowers' [] (n - 1) 1
            | head :: [] -> canPlaceFlowers' [] n head
            | 0 :: next :: tail when next = 0 && prev = 0 -> canPlaceFlowers' (next :: tail) (n - 1) 1
            | m :: next :: tail -> canPlaceFlowers' (next :: tail) n m

    canPlaceFlowers' flowerbed n 0

canPlaceFlowers [ 1; 0; 0; 0; 1 ] 1
canPlaceFlowers [ 1; 0; 0; 0; 1 ] 2
canPlaceFlowers [ 0 ] 1
canPlaceFlowers [ 1 ] 1
canPlaceFlowers [ 0; 0; 0 ] 2
canPlaceFlowers [ 0; 0; 0 ] 1
canPlaceFlowers [ 0; 0; 0 ] 3
canPlaceFlowers [ 0; 0; 1; 0; 1 ] 1
