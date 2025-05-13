open Ast

let pe_prim1 Neg e = match e with
  | Int i -> Int (-i)
  | e -> Prim1 {op = Neg; e}

let pe_prim2 op l r = match l, r with
  | Int l, Int r -> (match op with
    | Add -> Int (l + r)
    | Sub -> Int (l - r))
  | l, r -> Prim2 {op; l; r}

let rec pe_expr = function
  | Prim1 {op; e} -> pe_prim1 op (pe_expr e)
  | Prim2 {op; l; r} -> pe_prim2 op (pe_expr l) (pe_expr r)
  | e -> e

let%test_unit "pe_lint" =
  let pe_str str = pe_expr (Parser.expr_of_string str) in
  assert (pe_str "(+ 10 (- (+ 5 3)))" = Int 2);
  assert (pe_str "(- (+ 3 (- 5)))" = Int 2)
