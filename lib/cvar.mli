module In = Lvar_mon

type atom = In.atom

type prim0 = In.prim0

type prim1 = In.prim1

type prim2 = In.prim2

type expr =
  | Atom of atom
  | Prim0 of prim0
  | Prim1 of prim1
  | Prim2 of prim2

type stmt = Assign of {v: string; arg: expr}

type label = string

type tail = Return of expr | Seq of stmt * tail

type info = {locals: string list}

type program = {info: info; body: (label * tail) list}

val explicate_control : Lvar_mon.program -> program
