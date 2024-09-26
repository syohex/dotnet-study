type MyCalendar =
    { Ranges: (int * int) list }

    static member Empty: MyCalendar = { Ranges = [] }

    static member Book (start: int) (end': int) (cal: MyCalendar) : (bool * MyCalendar) =
        let ok = cal.Ranges |> List.forall (fun (s, e) -> start >= e || end' < s)

        if ok then
            true,
            { cal with
                Ranges = (start, end') :: cal.Ranges }
        else
            false, cal

let c = MyCalendar.Empty
let ok1, c1 = MyCalendar.Book 10 20 c
let ok2, c2 = MyCalendar.Book 15 25 c1
let ok3, _ = MyCalendar.Book 20 30 c2

// true, true, false
ok1, ok2, ok3
