type op2 = Add | Sub

type 'a program = {body: 'a}

(* ---- LInt ---- *)

module LInt = struct
  type 'a expr =
    [ `Int of int
    | `Read
    | `Neg of 'a
    | `Prim2 of op2 * 'a * 'a
    ]
end

(* ---- LVar ---- *)

module LVar = struct
  type 'a expr =
    [ `Var of string
    | `Let of string * 'a * 'a
    | 'a LInt.expr
    ]
end
