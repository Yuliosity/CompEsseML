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

let rec explicate_assign v e cont = match e with
  | In.Atom a -> Seq (Assign {v; arg = Atom a}, cont)
  | In.Let {v = v1; e = e1; body} ->
    explicate_assign v1 e1 (explicate_assign v body cont)
  | In.Prim0 p -> Seq (Assign {v; arg = Prim0 p}, cont)
  | In.Prim1 p -> Seq (Assign {v; arg = Prim1 p}, cont)
  | In.Prim2 p -> Seq (Assign {v; arg = Prim2 p}, cont)

let rec explicate_tail : In.expr -> tail = function
  | In.Atom a -> Return (Atom a)
  | In.Let {v; e; body} ->
    explicate_assign v e (explicate_tail body)
  | In.Prim0 op -> Return (Prim0 op)
  | In.Prim1 p -> Return (Prim1 p)
  | In.Prim2 p -> Return (Prim2 p)

(* Exercise 2.4 *)
let explicate_control {In.body} =
  {info = {locals = []}; body = ["start", explicate_tail body]}

let%test "CVar simple" =
  let p = {In.body = In.Prim2 {op = Ast.Add; l = In.Var "x"; r = In.Var "x"}} in
  let p = explicate_control p in
  p.body = [("start", Return (Prim2 {op = Add; l = Var "x"; r = Var "x"}))]

let%test "CVar 2" =
  let p = {
    In.body = In.Let {
      v = "y"; e = In.Let {
        v = "x_1"; e = In.Atom (In.Int 20); body = In.Let {
          v = "x_2"; e = In.Atom (In.Int 22); body = In.Prim2 {
            op = Ast.Add; l = In.Var "x_1"; r = In.Var "x_2"
          }
        }
      };
      body = In.Atom (In.Var "y")
    }
  } in
  let p = explicate_control p in
  p.body = [
    "start", Seq (
      Assign {v = "x_1"; arg = Atom (Int 20)},
      Seq (
        Assign {v = "x_2"; arg = Atom (Int 22)},
        Seq (
          Assign {v = "y"; arg = Prim2 {op = Add; l = Var "x_1"; r = Var "x_2"}},
          Return (Atom (Var "y"))
        )
      )
    )
  ]
