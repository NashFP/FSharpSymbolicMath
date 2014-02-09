// F# Symbolic Math
// See related blog post at http://luketopia.net/2013/07/28/fsharp-symbolic-math/

// The expression type
type Expr =
    | Con of int
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Div of Expr * Expr
    | Power of Expr * Expr
    | Neg of Expr

// Operators
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

// Derivitive function
let rec deriv var expr = 
    let d = deriv var
    match expr with
    | Var var -> Con 1                            // Identity Rule
    | Con x -> Con 0                              // Constant Rule
    | Mult(Con x, y) | Mult(y, Con x) -> Con x    // Constant Factor Rule
    | Add(x, y) -> d x + d y                      // Sum Rule
    | Sub(x, y) -> d x - d y                      // Difference Rule
    | Mult(x, y) -> d x * y + x * d y             // Product Rule
    | Div(x, y) -> (d x * y - x * d y) / y ** 2   // Quotient Rule
    | Power(var, Con x) -> x * var ** (x - 1)     // Elementary Power Rule
    | _ -> failwith "Sorry, don't know how to differentiate that!"
    
// Some variables
let x = Var("x")
let y = Var("y")
let z = Var("z")

// Example: just echo an expression
x ** 3 + x

// Example: take the derivitive of x ** 3 + x with respect to x
deriv x (x ** 3 + x)

