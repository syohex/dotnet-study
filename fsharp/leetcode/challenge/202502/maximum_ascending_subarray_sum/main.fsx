let maxAscendingSum (nums: int list) : int =
    let rec maxAscendingSum' nums prev sum acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let sum = if prev < h then sum + h else h
            maxAscendingSum' t h sum (max acc sum)

    match nums with
    | [] -> failwith "never reach here"
    | h :: t -> maxAscendingSum' t h h h

// 65
maxAscendingSum [ 10; 20; 30; 5; 10; 50 ]

// 150
maxAscendingSum [ 10; 20; 30; 40; 50 ]

// 33
maxAscendingSum [ 12; 17; 15; 13; 10; 11; 12 ]

// 100
maxAscendingSum [ 100; 10; 1 ]
