open Ast

(* ---- Interpreter ---- *)

module StrMap = Map.Make(String)

type env = int StrMap.t

module type INTERP = sig
  type expr
  (* type program = {body: expr} *)
  val interp_expr : env -> expr -> int
  (* val interp : program -> int *)
end

module InterpLInt = struct
  let interp_expr_lint base (env:env) = function
    | `Read -> read_int ()
    | `Int i -> i
    | `Neg arg -> -(base env arg)
    | `Prim2 (op, l, r) -> begin
      let l = base env l in
      let r = base env r in
      match op with
        | Add -> l + r
        | Sub -> l - r
      end
  let rec interp_expr e = interp_expr_lint interp_expr e

  let interp pr = interp_expr StrMap.empty pr.body
end

let%test_unit "LInt" =
  let open InterpLInt in
  let eval_expr e = interp {body = e} in
  assert (eval_expr (`Prim2 (Add, `Int 32, `Int 10)) = 42);
  assert (eval_expr (`Prim2 (Sub, `Int 52, `Prim2 (Add, `Int 5, `Int 5))) = 42);

module InterpLVar = struct
  let interp_expr_lvar base env = function
      | `Var v -> StrMap.find v env
      | `Let (v, e, body) ->
        let res = base env e in
        base (StrMap.add v res env) body
      | #LInt.expr as e -> InterpLInt.interp_expr_lint base env e
  let rec interp_expr e = interp_expr_lvar interp_expr e

  let interp pr = interp_expr StrMap.empty pr.body
end

let%test_unit "LVar" =
  let open InterpLVar in
  let eval_expr e = interp {body = e} in
  assert (eval_expr (`Let ("x", `Int 10, `Var "x")) = 10);
  assert (eval_expr (`Let ("x", `Let ("y", `Int 10, `Var "y"), `Var "x")) = 10);
  let test3 = `Prim2 (Add, `Int 10, `Let ("y", `Int 10, `Var "y")) in
  assert (eval_expr test3 = 20)
