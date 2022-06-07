let merge (nums1: int []) (m: int) (nums2: int []) (n: int) : unit =
    let rec merge' (nums1: int []) (m: int) (nums2: int []) (n: int) : int [] =
        if m = 0 && n = 0 then
            nums1
        else
            let index = m + n - 1

            match m > 0, n > 0 with
            | true, true ->
                if nums1.[m - 1] >= nums2.[n - 1] then
                    nums1.[index] <- nums1.[m - 1]
                    merge' nums1 (m - 1) nums2 n
                else
                    nums1.[index] <- nums2.[n - 1]
                    merge' nums1 m nums2 (n - 1)
            | true, false ->
                nums1.[index] <- nums1.[m - 1]
                merge' nums1 (m - 1) nums2 n
            | false, true ->
                nums1.[index] <- nums2.[n - 1]
                merge' nums1 m nums2 (n - 1)
            | false, false -> failwith "never reach here"

    let _ = merge' nums1 m nums2 n
    ()

let nums1_1 = [| 1; 2; 3; 0; 0; 0 |]
let nums2_1 = [| 2; 5; 6 |]
merge nums1_1 3 nums2_1 3
// [|1;2;2;3;5;6|]
nums1_1

let nums1_2 = [| 1 |]
let nums2_2 = [||]
merge nums1_2 1 nums2_2 0
// [|1|]
nums1_2

let nums1_3 = [| 0 |]
let nums2_3 = [| 1 |]
merge nums1_3 0 nums2_3 1
// [|1|]
nums1_3
