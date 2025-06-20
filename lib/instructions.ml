open Asm

module In = Cvar

type info = In.info

type program = {info: info; body: (label * block) list}

let atom a : arg = match a with
  | In.Int i -> `Imm i
  | In.Var v -> `Var v

let process_assign v expr = 
  let movv arg = Movq (v, arg) in
  let opv op arg = match op with
    | Ast.Add -> Addq (v, arg)
    | Ast.Sub -> Subq (v, arg)
  in
  match expr with
  | In.Atom a -> [movv (atom a)]
  | In.Prim0 Ast.Read -> [Callq "read_int"; movv `RAX]
  | In.Prim1 {op = Ast.Neg; arg = arg2} -> [Negq (atom arg2)]
  | In.Prim2 {op; l; r = In.Var v2}
    when v = `Var v2 -> [opv op (atom l)]
  | In.Prim2 {op; l; r} -> [movv (atom l); opv op (atom r)]

let process_stmt = function
  | In.Assign {v; arg} -> process_assign (`Var v) arg

let rec process_tail = function
  | In.Return e -> process_assign `RAX e @ [Jmp "conclusion"]
  | In.Seq (stmt, rest) -> process_stmt stmt @ process_tail rest

let process_section (label, tail) = (label, process_tail tail)

(* Exercize 2.5 *)
let select_instructions (pr:In.program) = {info = pr.info; body = List.map process_section pr.body}
