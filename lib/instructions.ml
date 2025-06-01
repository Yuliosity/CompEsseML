type reg = [`RAX | `RBX]

let print_reg (reg: reg) = match reg with
  | `RAX -> "%rax"
  | `RBX -> "%rbx"

type arg = [`Imm of int | reg | `Var of string]

let print_arg arg = match arg with
  | `Imm i -> string_of_int i
  | `Var v -> v
  | #reg as r -> print_reg r

type op2 = Movq | Addq | Subq

type label = Cvar.label

type instr
  = Movq of arg * arg
  | Addq of arg * arg
  | Subq of arg * arg
  | Callq of label
  | Jmp of label

let print_instr instr =
  let print_args (src, dst) = print_arg src ^ ", " ^ print_arg dst in
  match instr with
  | Movq (src, dst) -> "mov " ^ print_args (src, dst)
  | Addq (src, dst) -> "addq " ^ print_args (src, dst)
  | Subq (src, dst) -> "subq " ^ print_args (src, dst)
  | Callq label -> "callq " ^ label
  | Jmp label -> "jmp " ^ label

module In = Cvar

type info = In.info

type section = instr list

type program = {info: info; body: (label * section) list}

let atom = function
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
  | In.Prim1 {op = Ast.Neg; arg = arg2} -> [movv (`Imm 0); opv Ast.Sub (atom arg2)]
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
