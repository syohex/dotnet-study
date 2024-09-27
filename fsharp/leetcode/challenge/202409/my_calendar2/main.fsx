type MyCalendarTwo =
    { Bookings: (int * int) list
      Overlapped: (int * int) list }

    static member Empty: MyCalendarTwo = { Bookings = []; Overlapped = [] }

    static member Book (start': int) (end': int) (c: MyCalendarTwo) : bool * MyCalendarTwo =
        let isOverlapped (s1, e1) (s2, e2) = (max s1 s2) < (min e1 e2)

        let rec isTripleBookings overlapped =
            match overlapped with
            | [] -> false
            | range :: t ->
                if isOverlapped range (start', end') then
                    true
                else
                    isTripleBookings t

        let rec checkOverlapped bookings c =
            match bookings with
            | [] -> c
            | (s, e) :: t ->
                if isOverlapped (s, e) (start', end') then
                    checkOverlapped
                        t
                        { c with
                            Overlapped = (max s start', min e end') :: c.Overlapped }
                else
                    checkOverlapped t c

        if isTripleBookings c.Overlapped then
            false, c
        else
            let c' = checkOverlapped c.Bookings c

            true,
            { c' with
                Bookings = (start', end') :: c.Bookings }

let c = MyCalendarTwo.Empty
let ok1, c1 = MyCalendarTwo.Book 10 20 c
let ok2, c2 = MyCalendarTwo.Book 50 60 c1
let ok3, c3 = MyCalendarTwo.Book 10 40 c2
let ok4, c4 = MyCalendarTwo.Book 5 15 c3
let ok5, c5 = MyCalendarTwo.Book 5 10 c4
let ok6, c6 = MyCalendarTwo.Book 25 55 c5

// true, true, true, false, true, true
ok1, ok2, ok3, ok4, ok5, ok6

c6
