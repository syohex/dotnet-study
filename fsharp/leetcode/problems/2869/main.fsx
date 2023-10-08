let minOperations (nums: int list) (k: int) : int =
    let rec minOperations' nums k s acc =
        if Set.count s = k then
            acc
        else
            match nums with
            | [] -> acc
            | h :: t ->
                if h > k then
                    minOperations' t k s (acc + 1)
                else
                    minOperations' t k (Set.add h s) (acc + 1)

    minOperations' nums k Set.empty 0

// 4
minOperations [ 3; 1; 5; 4; 2 ] 2

// 5
minOperations [ 3; 1; 5; 4; 2 ] 5

// 4
minOperations [ 3; 2; 5; 3; 1 ] 3

// 40
minOperations
    [ 3
      28
      33
      26
      34
      20
      27
      5
      21
      23
      4
      21
      37
      35
      32
      15
      14
      1
      7
      2
      9
      6
      38
      17
      30
      18
      16
      13
      24
      29
      12
      14
      8
      36
      11
      31
      25
      22
      10
      19 ]
    38
