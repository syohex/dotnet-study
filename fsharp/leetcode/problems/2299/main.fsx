open System

let strongPasswordChecker2 (password: string) : bool =
    let rec strongPasswordChecker2' (cs: char list) prev lower upper digit special =
        match cs with
        | [] -> lower && upper && digit && special
        | h :: t ->
            if h = prev then
                false
            else
                strongPasswordChecker2'
                    t
                    h
                    (lower || Char.IsLower(h))
                    (upper || Char.IsUpper(h))
                    (digit || Char.IsDigit(h))
                    (special || "!@#$%^&*()-+".Contains(h))

    if password.Length < 8 then
        false
    else
        strongPasswordChecker2' (password |> Seq.toList) (char 255) false false false false

// true
strongPasswordChecker2 "IloveLe3tcode!"

// false
strongPasswordChecker2 "Me+You--IsMyDream"

// false
strongPasswordChecker2 "1aB!"
