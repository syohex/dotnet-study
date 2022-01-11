type Op =
    | Add
    | Sub
    | Mul
    | Div

    override this.ToString() =
        match this with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"

let valid op a b =
    match (op, a, b) with
    | (Add, _, _) -> true
    | (Sub, x, y) -> x > y
    | (Mul, _, _) -> true
    | (Div, x, y) -> x % y = 0

let apply op a b =
    match (op, a, b) with
    | (Add, x, y) -> x + y
    | (Sub, x, y) -> x - y
    | (Mul, x, y) -> x * y
    | (Div, x, y) -> x / y

type Expr =
    | Val of int
    | App of Op * Expr * Expr

    override this.ToString() =
        match this with
        | Val n -> n.ToString()
        | App (op, a, b) -> a.ToString() + op.ToString() + b.ToString()

let rec values (expr: Expr) : int list =
    match expr with
    | Val n -> [ n ]
    | App (_, left, right) -> values left @ values right

let rec eval (expr: Expr) : int =
    match expr with
    | Val n -> n
    | App (op, left, right) ->
        let lval = eval left
        let rval = eval right
        apply op lval rval

let expr1 = App(Add, Val(2), Val(3))
eval expr1
