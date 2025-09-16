module type LANG = sig
  type expr
  type program = {body: expr}
end

type op2 = Add | Sub

(* ---- LInt ---- *)

module LIntM (L: LANG) = struct
  type expr =
    [ `Int of int
    | `Read
    | `Neg of L.expr
    | `Prim2 of op2 * L.expr * L.expr
    ]

  type program = {body: expr}
end

module rec LInt : LANG
  with type expr = LIntM(LInt).expr
  = LIntM (LInt)

(* ---- LVar ---- *)

module LVarM (L: LANG) = struct
  module Base = LIntM (L)

  type expr =
    [ `Var of string
    | `Let of (string * L.expr * L.expr)
    | Base.expr]

  type program = {body: expr}
end

module rec LVar : (LANG with type expr = LVarM(LVar).expr) = LVarM (LVar)
