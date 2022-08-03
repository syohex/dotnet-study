type MyCalendar =
    { Ranges: (int * int) list }

    static member init() : MyCalendar = { Ranges = [] }

    static member book (start: int) (finish: int) (cal: MyCalendar) : (bool * MyCalendar) =
        let rec book' ranges start finish =
            match ranges with
            | [] -> true, { cal with Ranges = (start, finish) :: cal.Ranges }
            | (s, e) :: rest ->
                if s < finish && start < e then
                    false, cal
                else
                    book' rest start finish

        book' cal.Ranges start finish


let cal = MyCalendar.init ()
// true
let ret1, cal1 = MyCalendar.book 10 20 cal

// false
let ret2, cal2 = MyCalendar.book 15 25 cal1

// true
let ret3, _ = MyCalendar.book 20 30 cal2
