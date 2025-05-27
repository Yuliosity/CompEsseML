module In = Ast

type atom = Int of int | Var of string

type prim0 = In.prim0

type prim1op = Ast.prim1op = Neg

type prim1 = {op: prim1op; arg: atom}

type prim2op = Ast.prim2op = Add | Sub

type prim2 = {op: prim2op; l: atom; r: atom}

type expr =
  | Atom of atom
  | Prim0 of Ast.prim0
  | Prim1 of prim1
  | Prim2 of prim2
  | Let of {v: string; e: expr; body: expr}

type program = {body: expr}

val remove_complex_operands : Ast.expr -> expr
