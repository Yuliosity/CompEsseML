type atom = Int of int | Var of string

type expr =
  | Atom of atom
  | Prim0 of Ast.prim0
  | Prim1 of {op: Ast.prim1; arg: atom}
  | Prim2 of {op: Ast.prim2; l: atom; r: atom}
  | Let of {v: string; e: expr; body: expr}

type program = {body: expr}

val remove_complex_operands : Ast.expr -> expr
