let triangleType ((a, b, c): int * int * int) : string =
    if not <| (a + b > c && a + c > b && b + c > a) then "none"
    elif a = b && b = c then "equilateral"
    elif a = b || b = c || a = c then "isosceles"
    else "scalene"

// equilateral
triangleType (3, 3, 3)

// scalene
triangleType (3, 4, 5)

// none
triangleType (4, 4, 8)
