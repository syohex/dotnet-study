open System

let majorityElement (nums: int list) : int list =
    let rec findMajorities nums (num1, count1) (num2, count2) =
        match nums with
        | [] -> num1, num2
        | h :: t ->
            if h = num1 then
                findMajorities t (num1, count1 + 1) (num2, count2)
            elif h = num2 then
                findMajorities t (num1, count1) (num2, count2 + 1)
            elif count1 = 0 then
                findMajorities t (h, 1) (num2, count2)
            elif count2 = 0 then
                findMajorities t (num1, count1) (h, 1)
            else
                findMajorities t (num1, count1 - 1) (num2, count2 - 1)

    let rec countMajorities nums m1 m2 c1 c2 =
        match nums with
        | [] -> c1, c2
        | h :: t ->
            if h = m1 then countMajorities t m1 m2 (c1 + 1) c2
            elif h = m2 then countMajorities t m1 m2 c1 (c2 + 1)
            else countMajorities t m1 m2 c1 c2

    let m1, m2 = findMajorities nums (Int32.MinValue, 0) (Int32.MinValue, 0)
    let c1, c2 = countMajorities nums m1 m2 0 0

    let limit = nums.Length / 3

    match c1 > limit, c2 > limit with
    | true, true -> [ m1; m2 ]
    | true, false -> [ m1 ]
    | false, true -> [ m2 ]
    | _ -> []

// [3]
majorityElement [ 3; 2; 3 ]

// [1]
majorityElement [ 1 ]

// [1;2]
majorityElement [ 1; 2 ]
