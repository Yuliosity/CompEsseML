type prim0 = Read

type prim1 = Neg

type prim2 = Add | Sub

type expr =
  (* LInt *)
  | Int of int
  | Prim0 of prim0
  | Prim1 of {op: prim1; e: expr}
  | Prim2 of {op: prim2; l: expr; r: expr}
  (* LVar *)
  | Var of string
  | Let of {v: string; e: expr; body: expr}

type program = {body: expr}
