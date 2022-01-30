let rotate (nums: int list) (k: int) : int list =
    let revs = nums |> List.rev

    (revs |> List.take k |> List.rev)
    @ (revs |> List.skip k |> List.rev)

// [5, 6, 7, 1, 2, 3, 4]
rotate [ 1 .. 7 ] 3

// [3, 99, -1, -100]
rotate [ -1; -100; 3; 99 ] 2
