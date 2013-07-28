// F# Symbolic Math
// See related blog post at http://luketopia.net/2013/07/28/fsharp-symbolic-math/

// Type aliases
type Num = int
type Name = string

// The expression type
type Expr = 
    | Con of Num
    | Var of Name
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * Expr

// The relations type
type Relation =
    | Equal of Expr * Expr
    | NotEqual of Expr * Expr
    | Less of Expr * Expr
    | LessEqual of Expr * Expr
    | Greater of Expr * Expr
    | GreaterEqual of Expr * Expr

// Some variables
let x = Var("x")
let y = Var("y")
let z = Var("z")

// Save our operators for later
let add = (+)
let sub = (-)
let mult = (*)
let div = (/)
let pow = ( ** )
let equal = (=)
let notEqual = (<>)
let less = (<)
let lessEqual = (<=)
let greater = (>)
let greaterEqual = (>=)

// Arithmetic operators
let (+) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Add(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Add(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Add(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Add(a, b)
    | _ -> failwith "The + operator is not supported with those operand types."

let (-) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Sub(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Sub(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Sub(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Sub(a, b)
    | _ -> failwith "The - operator is not supported with those operand types."

let (*) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Mult(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Mult(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Mult(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Mult(a, b)
    | _ -> failwith "The * operator is not supported with those operand types."

let (/) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Div(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Div(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Div(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Div(a, b)
    | _ -> failwith "The / operator is not supported with those operand types."

let ( ** ) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Pow(Con(a), Con(b))
    | (:? Expr as a), (:? Num as b) -> Pow(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Pow(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Pow(a, b)
    | _ -> failwith "The ** operator is not supported with those operand types."

// Relational operators
let (=) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Equal(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Equal(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Equal(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Equal(a, b)
    | _ -> failwith "The = operator is not supported with those operand types."

let (<>) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> NotEqual(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> NotEqual(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> NotEqual(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> NotEqual(a, b)
    | _ -> failwith "The <> operator is not supported with those operand types."

let (<) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Less(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Less(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Less(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Less(a, b)
    | _ -> failwith "The < operator is not supported with those operand types."

let (<=) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> LessEqual(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> LessEqual(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> LessEqual(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> LessEqual(a, b)
    | _ -> failwith "The <= operator is not supported with those operand types."

let (>) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> Greater(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> Greater(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> Greater(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> Greater(a, b)
    | _ -> failwith "The > operator is not supported with those operand types."

let (>=) x y =
    match box x, box y with
    | (:? Num as a), (:? Num as b) -> GreaterEqual(Con(a),Con(b))
    | (:? Expr as a), (:? Num as b) -> GreaterEqual(a, Con(b))
    | (:? Num as a), (:? Expr as b) -> GreaterEqual(Con(a), b)
    | (:? Expr as a), (:? Expr as b) -> GreaterEqual(a, b)
    | _ -> failwith "The >= operator is not supported with those operand types."

// Function to take simple derivitives
let rec deriv var expr = 
    match expr with
    | Con(_) -> Con(0)                                      
    | Add(a, b) -> Add(deriv var a, deriv var b)           
    | Pow(var, Con(a)) -> Mult(Con(a), Pow(var, Con(sub a 1)))   
    | _ -> failwith "Sorry, don't know how to differentiate that!" 


// Example: just echo an expression
1 + x

// Example: take the derivitive of x ^ 3 + 1 with respect to x
deriv x (x ** 3 + 1)
