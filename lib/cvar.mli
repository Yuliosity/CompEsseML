module In = Lvar_mon

type atom = In.atom = Int of int | Var of string

type prim1 = In.prim1 = {op: Ast.prim1op; arg: atom}

type prim2 = In.prim2 = {op: Ast.prim2op; l: atom; r: atom}

type expr =
  | Atom of atom
  | Prim0 of Ast.prim0
  | Prim1 of prim1
  | Prim2 of prim2

type stmt = Assign of {v: string; arg: expr}

type label = string

type tail = Return of expr | Seq of stmt * tail

type info = {locals: string list}

type program = {info: info; body: (label * tail) list}

val explicate_control : Lvar_mon.program -> program
