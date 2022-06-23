#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let scheduleCourse (courses: (int * int) list) : int =
    let rec scheduleCourse' time courses q qLen =
        match courses with
        | [] -> qLen
        | (duration, lastDay) :: rest ->
            if time + duration <= lastDay then
                let q' = PriorityQueue.insert duration q
                scheduleCourse' (time + duration) rest q' (qLen + 1)
            else
                match PriorityQueue.tryPop q with
                | None -> scheduleCourse' time rest q qLen
                | Some (minDuration, q') ->
                    if minDuration > duration then
                        let time' = time - minDuration + duration
                        let q'' = PriorityQueue.insert duration q'
                        scheduleCourse' time' rest q'' qLen
                    else
                        scheduleCourse' time rest q qLen

    let courses' =
        courses
        |> List.sortWith (fun (_, lastDay1) (_, lastDay2) -> compare lastDay1 lastDay2)

    let q = PriorityQueue.empty true
    scheduleCourse' 0 courses' q 0

// 3
scheduleCourse [ (100, 200)
                 (200, 1300)
                 (1000, 1250)
                 (2000, 3200) ]

// 1
scheduleCourse [ (1, 2) ]

// 0
scheduleCourse [ (3, 2); (4, 3) ]

// 2
scheduleCourse [ (5, 5)
                 (4, 6)
                 (2, 6) ]

// 4
scheduleCourse [ (7, 17)
                 (3, 12)
                 (10, 20)
                 (9, 10)
                 (5, 20)
                 (10, 19)
                 (4, 18) ]
