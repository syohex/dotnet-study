let findMaxForm (strs: string list) (m: int) (n: int) : int =
    let rec findMaxForm' pos zeroOne m n cache =
        match zeroOne with
        | [] -> 0, cache
        | (zeros, ones) :: t ->
            let key = (pos, m, n)

            match Map.tryFind key cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache =
                    if m - zeros >= 0 && n - ones >= 0 then
                        let ret, cache = findMaxForm' (pos + 1) t (m - zeros) (n - ones) cache
                        ret + 1, cache
                    else
                        0, cache

                let ret2, cache = findMaxForm' (pos + 1) t m n cache
                let ret = max ret1 ret2
                ret, Map.add key ret cache

    let count s =
        s
        |> Seq.fold (fun (zeros, ones) c -> if c = '0' then zeros + 1, ones else zeros, ones + 1) (0, 0)

    let zeroOne = strs |> List.map count

    findMaxForm' 0 zeroOne m n Map.empty |> fst

// 4
findMaxForm [ "10"; "0001"; "111001"; "0"; "1" ] 5 3

// 2
findMaxForm [ "10"; "1"; "0" ] 1 1

let test =
    [ "011"
      "1"
      "11"
      "0"
      "010"
      "1"
      "10"
      "1"
      "1"
      "0"
      "0"
      "0"
      "01111"
      "011"
      "11"
      "00"
      "11"
      "10"
      "1"
      "0"
      "0"
      "0"
      "0"
      "101"
      "001110"
      "1"
      "0"
      "1"
      "0"
      "0"
      "10"
      "00100"
      "0"
      "10"
      "1"
      "1"
      "1"
      "011"
      "11"
      "11"
      "10"
      "10"
      "0000"
      "01"
      "1"
      "10"
      "0" ]
// 45
findMaxForm test 44 39
