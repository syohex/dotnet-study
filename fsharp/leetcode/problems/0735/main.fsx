let asteroidCollision (asteroids: int list) : int list =
    let rec collideCheck asteroid stack =
        match stack with
        | [] -> [ asteroid ]
        | h :: t ->
            if h > 0 && asteroid < 0 then
                if h > -asteroid then stack
                elif h = -asteroid then t
                else collideCheck asteroid t
            else
                asteroid :: stack

    let rec asteroidCollision' asteroids stack =
        match asteroids with
        | [] -> List.rev stack
        | h :: t ->
            match stack with
            | [] -> asteroidCollision' t (h :: stack)
            | _ ->
                let stack' = collideCheck h stack
                asteroidCollision' t stack'

    asteroidCollision' asteroids []

// [5, 10]
asteroidCollision [ 5; 10; -5 ]

// []
asteroidCollision [ 8; -8 ]

// [10]
asteroidCollision [ 10; 2; -5 ]
