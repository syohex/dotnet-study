type CustomStack =
    { Data: int[]
      Top: int }

    static member Init(maxSize: int) : CustomStack =
        { Data = Array.zeroCreate maxSize
          Top = -1 }

    static member Push (x: int) (s: CustomStack) : CustomStack =
        if s.Top + 1 < s.Data.Length then
            let top = s.Top + 1
            s.Data.[top] <- x
            { s with Data = s.Data; Top = top }
        else
            s

    static member Pop(s: CustomStack) : int * CustomStack =
        if s.Top >= 0 then
            let ret = s.Data.[s.Top]
            ret, { s with Top = s.Top - 1 }
        else
            -1, s

    static member Increment (k: int) (v: int) (s: CustomStack) : CustomStack =
        let len = min k (s.Top + 1)
        seq { 0 .. (len - 1) } |> Seq.iter (fun i -> s.Data.[i] <- s.Data.[i] + v)
        { s with Data = s.Data }

let s = CustomStack.Init 3
let s1 = CustomStack.Push 1 s
let s2 = CustomStack.Push 2 s1
let v3, s3 = CustomStack.Pop s2
let s4 = CustomStack.Push 2 s3
let s5 = CustomStack.Push 3 s4
let s6 = CustomStack.Push 4 s5
let s7 = CustomStack.Increment 5 100 s6
let s8 = CustomStack.Increment 2 100 s7
let v9, s9 = CustomStack.Pop s8
let v10, s10 = CustomStack.Pop s9
let v11, s11 = CustomStack.Pop s10
let v12, _ = CustomStack.Pop s11

// 2, 103, 202, 201, -1
v3, v9, v10, v11, v12
