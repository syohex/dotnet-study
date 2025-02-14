type ProductOfNumbers =
    { nums: ResizeArray<int> }

    static member empty() : ProductOfNumbers = { nums = ResizeArray [ 1 ] }

    static member add (num: int) (self: ProductOfNumbers) : ProductOfNumbers =
        if num = 0 then
            { nums = ResizeArray [ 1 ] }
        else
            self.nums.Add (num * self.nums.[self.nums.Count - 1])
            self

    static member getProduct (k: int) (self: ProductOfNumbers) : int =
        if k >= self.nums.Count then
            0
        else
            let len = self.nums.Count
            self.nums.[len - 1] / self.nums.[len - 1 - k]

let pn =
    ProductOfNumbers.empty ()
    |> ProductOfNumbers.add 3
    |> ProductOfNumbers.add 0
    |> ProductOfNumbers.add 2
    |> ProductOfNumbers.add 5
    |> ProductOfNumbers.add 4

let ret1 = ProductOfNumbers.getProduct 2 pn
let ret2 = ProductOfNumbers.getProduct 3 pn
let ret3 = ProductOfNumbers.getProduct 4 pn
let ret4 = pn |> ProductOfNumbers.add 8 |> ProductOfNumbers.getProduct 2

// 20, 40, 0, 32
printfn "%d, %d, %d, %d" ret1 ret2 ret3 ret4
