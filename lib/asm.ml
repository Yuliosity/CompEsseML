type reg =
  [ `RSP | `RBP | `RAX | `RBX | `RCX | `RDX | `RSI | `RDI
  | `R8 | `R9 | `R10 | `R11 | `R12 | `R13 | `R14 | `R15
  ]

let print_reg (reg: reg) = match reg with
  | `RSP -> "%rsp"
  | `RBP -> "%rbp"
  | `RAX -> "%rax"
  | `RBX -> "%rbx"
  | `RCX -> "%rcx"
  | `RDX -> "%rdx"
  | `RSI -> "%rsi"
  | `RDI -> "%rdi"
  | `R8 -> "%r8"
  | `R9 -> "%r9"
  | `R10 -> "%r10"
  | `R11 -> "%r11"
  | `R12 -> "%r12"
  | `R13 -> "%r13"
  | `R14 -> "%r14"
  | `R15 -> "%r15"

type arg = [`Imm of int | reg | `Var of string | `Deref of reg * int]

let print_arg arg = match arg with
  | `Imm i -> string_of_int i
  | `Var v -> v
  | `Deref (r, i) -> print_reg r ^ "(" ^ string_of_int i ^ ")"
  | #reg as r -> print_reg r

type label = Cvar.label

type instr
  = Movq of arg * arg
  | Addq of arg * arg
  | Subq of arg * arg
  | Negq of arg
  | Callq of label
  | Retq
  | Pushq of arg
  | Popq of arg
  | Jmp of label

type block = instr list

let print_instr instr =
  let print_args (src, dst) = print_arg src ^ ", " ^ print_arg dst in
  match instr with
  | Movq (src, dst) -> "mov " ^ print_args (src, dst)
  | Addq (src, dst) -> "addq " ^ print_args (src, dst)
  | Subq (src, dst) -> "subq " ^ print_args (src, dst)
  | Negq arg -> "negq " ^ print_arg arg
  | Callq label -> "callq " ^ label
  | Retq -> "retq"
  | Pushq arg -> "pushq " ^ print_arg arg
  | Popq arg -> "popq " ^ print_arg arg
  | Jmp label -> "jmp " ^ label
