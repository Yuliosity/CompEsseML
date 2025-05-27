type prim0 = Read

type prim1op = Neg

type prim2op = Add | Sub

type expr =
  (* LInt *)
  | Int of int
  | Prim0 of prim0
  | Prim1 of {op: prim1op; arg: expr}
  | Prim2 of {op: prim2op; l: expr; r: expr}
  (* LVar *)
  | Var of string
  | Let of {v: string; e: expr; body: expr}

type program = {body: expr}
