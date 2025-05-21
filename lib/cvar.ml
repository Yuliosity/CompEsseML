type atom = Lvar_mon.atom

type expr =
  | Atom of atom
  | Prim0 of Ast.prim0
  | Prim1 of {op: Ast.prim1; arg: atom}
  | Prim2 of {op: Ast.prim2; l: atom; r: atom}

type stmt = Assign of {v: string; arg: expr}

type label = string

type block = {label: label; stmts: stmt list; return: expr}

type info = {locals: string list}

type program = {info: info; body: block list}
