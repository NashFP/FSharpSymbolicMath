// F# Symbolic Math
// See related blog post at http://luketopia.net/2013/07/28/fsharp-symbolic-math/

type Expr =
    | Con of int
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Div of Expr * Expr
    | Power of Expr * Expr
    | Neg of Expr

type Expr with
    static member (+) (x, y) = Add(x, y)
    static member (+) (x, y) = Add(x, Con y)
    static member (+) (x, y) = Add(Con x, y)
    static member (-) (x, y)  = Sub(x, y)
    static member (-) (x, y) = Sub(x, Con y)
    static member (-) (x, y) = Sub(Con x, y)
    static member (*) (x, y)  = Mult(x, y)
    static member (*) (x, y) = Mult(x, Con y)
    static member (*) (x, y) = Mult(Con x, y)
    static member (/) (x, y)  = Div(x, y)
    static member (/) (x, y) = Div(x, Con y)
    static member (/) (x, y) = Div(Con x, y)
    static member Pow (x, y) = Power(x, y)
    static member Pow (x, y) = Power(x, Con y)
    static member Pow (x, y) = Power(Con x, y)
    static member (~-) x = Neg x
    static member (~+) x = x

let rec deriv var expr = 
    let d = deriv var
    match (expr:Expr) with
    | Var var -> Con 1                            // Identity Rule
    | Con x -> Con 0                              // Constant Rule
    | Mult(Con x, y) | Mult(y, Con x) -> Con x    // Constant Factor Rule
    | Add(x, y) -> d x + d y                      // Sum Rule
    | Sub(x, y) -> d x - d y                      // Difference Rule
    | Mult(x, y) -> d x * y + x * d y             // Product Rule
    | Div(x, y) -> (d x * y - x * d y) / y ** 2   // Quotient Rule
    | Power(var, Con x) -> x * var ** (x - 1)     // Elementary Power Rule
    | _ -> failwith "Sorry, don't know how to differentiate that!"

type Associativity = Left | Right

[<RequireQualifiedAccess>]
type BinaryOp = 
    | Add 
    | Sub 
    | Mult 
    | Div 
    | Power

type BinaryOp with
    member this.Symbol =
        match this with
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Power -> "**"
    member this.Precedence =
        match this with
        | Add | Sub -> 1
        | Mult | Div -> 2
        | Power -> 3
    member this.Associativity =
        match this with
        | Add | Mult -> None 
        | Sub | Div -> Some Left
        | Power -> Some Right

[<RequireQualifiedAccess>]
type UnaryOp = 
    | Neg

type UnaryOp with
    member this.Symbol =
        match this with
        | Neg -> "-"

let (|Binary|Unary|Variable|Constant|) expr =
    match expr with
    | Add(x, y) -> Binary(BinaryOp.Add, x, y)
    | Sub(x, y) -> Binary(BinaryOp.Sub, x, y)
    | Mult(x, y) -> Binary(BinaryOp.Mult, x, y)
    | Div(x, y) -> Binary(BinaryOp.Div, x, y)
    | Power(x, y) -> Binary(BinaryOp.Power, x, y)
    | Neg(x) -> Unary(UnaryOp.Neg, x)
    | Var(x) -> Variable(x)
    | Con(x) -> Constant(x)

let rec print expr =
    let parensPrint innerExpr = sprintf "(%s)" (print innerExpr)
    match expr with
    | Binary(op, left, right) ->
        let printInner defAssoc innerExpr = 
            let opAssoc = defaultArg op.Associativity defAssoc
            match innerExpr with
            | Binary(innerOp, _, _) when innerOp.Precedence < op.Precedence -> parensPrint innerExpr
            | Binary(innerOp, _, _) when innerOp.Precedence = op.Precedence && opAssoc <> defAssoc -> parensPrint innerExpr
            | _ -> print innerExpr
        sprintf "%s %s %s" (printInner Left left) op.Symbol (printInner Right right)
    | Unary(op, operand) ->
        match expr with
        | Neg(Var _) -> print operand
        | Neg(Con x) when x >= 0 -> print operand
        | _ -> parensPrint operand
        |> sprintf "%s%s" op.Symbol
    | Variable x -> x
    | Constant x -> string x

let x = Var "x"
let y = Var "y"
let z = Var "z"
