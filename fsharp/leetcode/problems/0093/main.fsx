let restoreIpAddresses (s: string) : string list =
    let rec f nums len (acc: int list) ret =
        match nums with
        | [] ->
            if len = 4 then
                let s = System.String.Join(".", (acc |> List.rev |> List.map string))
                s :: ret
            else
                ret
        | h :: t ->
            if len >= 4 then
                ret
            elif h = 0 then
                f t (len + 1) (0 :: acc) ret
            else
                let ret3 =
                    match nums with
                    | d1 :: d2 :: d3 :: rest ->
                        let num = 100 * d1 + 10 * d2 + d3

                        if num <= 255 then
                            f rest (len + 1) (num :: acc) ret
                        else
                            ret
                    | _ -> ret

                let ret2 =
                    match nums with
                    | d1 :: d2 :: rest ->
                        let num = + 10 * d1 + d2
                        f rest (len + 1) (num :: acc) ret3
                    | _ -> ret3

                match nums with
                | d1 :: rest -> f rest (len + 1) (d1 :: acc) ret2
                | _ -> failwith "never reach here"

    let nums =
        s
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toList

    f nums 0 [] []

// ["255.255.11.135","255.255.111.35"]
restoreIpAddresses "25525511135"

// ["0.0.0.0"]
restoreIpAddresses "0000"

// ["1.0.10.23","1.0.102.3","10.1.0.23","10.10.2.3","101.0.2.3"]
restoreIpAddresses "101023"
