open Sexplib

open Ast
let rec expr_of_sexp : Sexp.t -> expr = let open Sexp in function
  | List [Atom "+"; l; r] -> Prim2 {op = Add; l = expr_of_sexp l; r = expr_of_sexp r}
  | List [Atom "-"; l; r] -> Prim2 {op = Sub; l = expr_of_sexp l; r = expr_of_sexp r}
  | List [Atom "-"; e] -> Prim1 {op = Neg; e = expr_of_sexp e}
  | List [Atom "read"] -> Prim0 Read
  | List [Atom "let"; List [List [Atom v; e]]; body] ->
    Let {v; e = expr_of_sexp e; body = expr_of_sexp body}
  | Atom v -> (match int_of_string_opt v with
    | None -> Var v
    | Some i -> Int i)
  | s -> failwith @@ Sexp.to_string s ^ " is not an expression"

let expr_of_string s = expr_of_sexp (Sexp.of_string s)

let program_of_sexp body = {body = expr_of_sexp body}

let%test_unit "LInt" =
  assert (expr_of_string "(+ 10 20)" =
    Prim2 {op = Add; l = Int 10; r = Int 20});
  assert (expr_of_string "(- 10 (- 20))" =
    Prim2 {op = Sub; l = Int 10; r = Prim1 {op = Neg; e = Int 20}})

let%test_unit "LVar" =
  assert (expr_of_string "(let ((x 10)) (+ x x))" =
    Let {v = "x"; e = Int 10; body = Prim2 {op = Add; l = Var "x"; r = Var "x"}})
