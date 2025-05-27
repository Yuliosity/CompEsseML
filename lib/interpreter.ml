module StrMap = Map.Make(String)

type env = int StrMap.t

open Ast

class interp_lint = object (self)
  method interp_expr (env: env) : Ast.expr -> int = function
    | Prim0 Read -> read_int ()
    | Int i -> i
    | Prim1 {op = Neg; arg} -> -(self#interp_expr env arg)
    | Prim2 {op; l; r} -> begin
      let l = self#interp_expr env l in
      let r = self#interp_expr env r in
      match op with
        | Add -> l + r
        | Sub -> l - r
      end
    | _ -> failwith "Unreachable: interp_exp"
  method interp (p: Ast.program) =
    self#interp_expr StrMap.empty p.body
end

let%test_unit "LInt" =
  let interp = new interp_lint in
  let eval_expr e = interp#interp {body = e} in
  assert (eval_expr (Prim1 {op = Neg; arg = Int 10}) = -10);
  assert (eval_expr (Prim2 {op = Add; l = Int 32; r = Int 10}) = 42);
  let test3 = Prim2 {op = Sub; l = Int 52; r = Prim2 {op = Add; l = Int 5; r = Int 5}} in
  assert (eval_expr test3 = 42)

class interp_lvar = object (self)
  inherit interp_lint as super
  method! interp_expr (env: env) : Ast.expr -> int = function
    | Var v -> StrMap.find v env
    | Let {v; e; body} ->
      let res = self#interp_expr env e in
      self#interp_expr (StrMap.add v res env) body
    | e -> super#interp_expr env e
end

let%test_unit "LVar" =
  let interp = new interp_lvar in
  let eval_expr e = interp#interp {body = e} in
  assert (eval_expr (Prim2 {op = Add; l = Int 32; r = Int 10}) = 42);
  let test2 =
    Let {
      v = "aa";
      e = Prim2 {op = Add; l = Int 20; r = Int 1};
      body = Prim2 {op = Add; l = Var "aa"; r = Var "aa"}
    }
  in
  assert (eval_expr test2 = 42)
